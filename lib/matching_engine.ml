(** The matching engine.

    Consumes incoming orders and matches them against the resting {!Order_book}
    under price-time priority, producing fills (including partial fills) and
    resting remainders. This is the active counterpart to the passive book: the
    book stores and orders resting liquidity, and this module decides what
    crosses.

    {2 Priority}

    The book already exposes its best bid/ask with price-time priority baked in
    (best price first, and within a price level the earliest timestamp first —
    arrival order breaking exact ties), so the engine simply repeatedly consumes
    the best opposing order. An incoming buy lifts the lowest asks; an
    incoming sell hits the highest bids. Execution always happens at the {e
    resting} order's price, so an aggressive order that improves on the book is
    filled at the better resting price.

    {2 State}

    Like the book, the engine is purely functional: {!process} takes a book and
    an order and returns a {e new} book plus the fills produced and a typed
    {!outcome}. The input book is never mutated.

    {2 No crossed book}

    Matching stops as soon as the best opposing price is strictly worse than an
    incoming limit's price (a market order takes whatever liquidity exists and
    never rests). A limit's leftover therefore rests at a price that cannot cross
    what remains on the other side, so a book that was uncrossed before
    {!process} is uncrossed after it. *)

open Types

(** A single execution between an incoming (aggressor) order and one resting
    order. Quantities and prices are the typed domain values, so a fill can only
    describe a positive quantity at a positive price. *)
type fill = {
  resting_id : Order_id.t;  (** the resting order that supplied liquidity *)
  incoming_id : Order_id.t;  (** the aggressor whose arrival caused the trade *)
  price : Price_ticks.t;  (** execution price = the resting order's price *)
  quantity : Quantity.t;  (** units traded in this fill, always [> 0] *)
  incoming_side : side;  (** side of the aggressor ([Buy] lifts asks, [Sell] hits bids) *)
}

(** What became of the incoming order once matching finished. *)
type outcome =
  | Filled  (** the incoming order executed in full; nothing rested *)
  | Rested of Quantity.t
      (** a limit order left this many units resting on the book (either it
          never crossed, or it partially filled and the remainder rested) *)
  | Discarded of Quantity.t
      (** a market order could not be fully filled; this many units went
          unfilled and were {e not} rested *)
  | Cancelled of Order_id.t  (** a cancel removed this resting target *)
  | Cancel_miss of Order_id.t  (** a cancel named a target that was not resting *)

(** The full result of processing one order. *)
type result = {
  book : Order_book.t;  (** the book after the order was processed *)
  fills : fill list;  (** fills in execution order (best/earliest first) *)
  outcome : outcome;
}

let string_of_outcome = function
  | Filled -> "filled"
  | Rested q -> Printf.sprintf "rested %s" (Quantity.pp q)
  | Discarded q -> Printf.sprintf "discarded %s" (Quantity.pp q)
  | Cancelled id -> Printf.sprintf "cancelled %s" (Order_id.pp id)
  | Cancel_miss id -> Printf.sprintf "cancel miss %s" (Order_id.pp id)

(* The best resting order on the side an aggressor of [incoming_side] trades
   against: a buy lifts asks, a sell hits bids. *)
let opposing_best book = function
  | Buy -> Order_book.best_ask book
  | Sell -> Order_book.best_bid book

(* Consume [qty] units from the best order on the side [incoming_side] trades
   against — the same order [opposing_best] just returned, since the book is
   unchanged between the two calls. The book exposes this best-order primitive
   rather than a raw reduce-by-id, so the engine never touches the book's
   internal id indexing. *)
let consume_opposing_best book incoming_side qty =
  match incoming_side with
  | Buy -> Order_book.reduce_best_ask book qty
  | Sell -> Order_book.reduce_best_bid book qty

(* Would an aggressor of [incoming_side] at [limit] accept a trade at the resting
   price [rprice]? A market order ([limit = None]) accepts any price; a limit buy
   accepts asks at or below its price, a limit sell accepts bids at or above it. *)
let price_acceptable incoming_side ~limit ~rprice =
  match limit with
  | None -> true
  | Some lim -> (
      match incoming_side with
      | Buy -> Price_ticks.compare rprice lim <= 0
      | Sell -> Price_ticks.compare rprice lim >= 0)

(* Match [remaining] units of an aggressor against the book, best opposing order
   first, until the aggressor is exhausted or no acceptable liquidity is left.
   Returns the fills (execution order), the resulting book, and the units still
   unfilled. [limit] is [None] for a market order. *)
let match_against book ~incoming_side ~incoming_id ~limit ~remaining =
  let rec loop book fills_rev remaining =
    if remaining = 0 then (List.rev fills_rev, book, 0)
    else
      match opposing_best book incoming_side with
      | None -> (List.rev fills_rev, book, remaining) (* no more liquidity *)
      | Some (Order.Market _ | Order.Cancel _) ->
          (* only limit orders ever rest, so this is unreachable *)
          assert false
      | Some (Order.Limit { id = resting_id; price = rprice; quantity = rqty; _ }) ->
          if not (price_acceptable incoming_side ~limit ~rprice) then
            (List.rev fills_rev, book, remaining)
          else
            let available = Quantity.to_int rqty in
            let traded = min remaining available in
            let traded_q =
              match Quantity.create traded with
              | Some q -> q (* traded = min of two positives, so > 0 *)
              | None -> assert false
            in
            let fill =
              { resting_id; incoming_id; price = rprice; quantity = traded_q; incoming_side }
            in
            (* Consuming the best opposing order removes it when [traded]
               exhausts it and otherwise shrinks it in place, keeping its
               time-priority slot. *)
            let book = consume_opposing_best book incoming_side traded_q in
            loop book (fill :: fills_rev) (remaining - traded)
  in
  loop book [] remaining

(** [process book order] matches [order] against [book] and returns the updated
    book, the fills produced, and a typed {!outcome}.

    - A [Limit] order matches against the opposing side at its price or better,
      then rests any unfilled remainder on the book.
    - A [Market] order matches against available liquidity and never rests; any
      unfillable remainder is discarded.
    - A [Cancel] removes its target if it is resting, producing no fills. *)
let process book (order : Order.t) : result =
  match order with
  | Order.Cancel { target; _ } -> (
      match Order_book.cancel book target with
      | Ok book -> { book; fills = []; outcome = Cancelled target }
      | Error (Order_book.Unknown_order_id _) ->
          { book; fills = []; outcome = Cancel_miss target })
  | Order.Market { id; side; quantity; _ } ->
      let fills, book, remaining =
        match_against book ~incoming_side:side ~incoming_id:id ~limit:None
          ~remaining:(Quantity.to_int quantity)
      in
      let outcome =
        match Quantity.create remaining with
        | None -> Filled (* remaining = 0 *)
        | Some q -> Discarded q (* market orders never rest the remainder *)
      in
      { book; fills; outcome }
  | Order.Limit { id; side; price; quantity; timestamp } ->
      let fills, book, remaining =
        match_against book ~incoming_side:side ~incoming_id:id ~limit:(Some price)
          ~remaining:(Quantity.to_int quantity)
      in
      if remaining = 0 then { book; fills; outcome = Filled }
      else
        (* Rest the unfilled remainder as a fresh resting limit at the aggressor's
           own price. It cannot cross: matching only stopped because the best
           opposing price was strictly worse than [price]. *)
        let remainder =
          match
            Order.limit ~id ~side ~price:(Price_ticks.to_int price) ~quantity:remaining
              ~timestamp
          with
          | Ok o -> o
          | Error _ -> assert false (* remaining > 0 and price was already valid *)
        in
        let book =
          match Order_book.add book remainder with
          | Ok b -> b
          | Error _ -> assert false (* id is the aggressor's, unique, not yet resting *)
        in
        let rested = match Quantity.create remaining with Some q -> q | None -> assert false in
        { book; fills; outcome = Rested rested }
