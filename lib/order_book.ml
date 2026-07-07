(** The limit order book (resting orders only; no matching).

    Maintains resting bids and asks under price-time priority: best price first,
    and within a price level the earliest {e timestamp} first. When two resting
    orders carry the same timestamp the tie is broken by arrival order, so the
    ordering is fully deterministic. This module is the passive container — it
    stores, indexes, and removes resting limit orders. It performs {e no}
    matching: an added order always rests, even if it would cross the book.
    Crossing is resolved later by {!Matching_engine}, so this module
    intentionally has no "no crossed book" assertion.

    {2 Representation}

    The book is immutable: every mutating operation returns a new {!t}. This
    mirrors the functional style of the core types, makes a book value a
    cheap, first-class snapshot (useful for deterministic replay and tests), and
    rules out aliasing bugs. The cost is allocation per update (the price maps
    rebalance and the priority-ordered level list is rebuilt on insert). A future
    matching hot loop may cache best prices and switch to a mutable level
    structure behind this same interface; the public signatures here are meant to
    survive that. *)

open Types

(* Price levels are kept in a map keyed by price. Bids and asks share the same
   ascending key ordering; the "best" end differs per side (highest bid, lowest
   ask) and is read via [max_binding]/[min_binding] respectively. *)
module Price_map = Map.Make (Price_ticks)

(* Secondary index from order id to the resting entry, for O(log N) lookup and
   cancellation without scanning price levels. Only [Limit] orders ever enter
   it. *)
module Id_map = Map.Make (Order_id)

(* A resting order together with the arrival sequence number the book stamped on
   it when it was added. Priority within a price level is the pair
   [(timestamp, seq)]: earliest timestamp first, and for equal timestamps the
   order that arrived first (smaller [seq]). [seq] is unique and monotonic per
   book, so [(timestamp, seq)] is a strict total order — the tie-break is always
   decided, and it is decided deterministically. *)
type resting = { order : Order.t; seq : int }

type t = {
  bids : resting list Price_map.t;
      (** price -> resting orders at that price, in priority order (earliest
          timestamp first, arrival order breaking ties) *)
  asks : resting list Price_map.t;
  index : resting Id_map.t;  (** id -> the resting entry it refers to *)
  next_seq : int;  (** arrival counter stamped on the next order added *)
}

let empty =
  {
    bids = Price_map.empty;
    asks = Price_map.empty;
    index = Id_map.empty;
    next_seq = 0;
  }

(** Why {!add} refused to rest an order. *)
type add_error =
  | Not_a_limit_order  (** a market or cancel order was passed to [add] *)
  | Duplicate_order_id of Order_id.t  (** an order with this id already rests *)

(** Why {!cancel} could not remove an order. *)
type cancel_error = Unknown_order_id of Order_id.t  (** no resting order has this id *)

let string_of_add_error = function
  | Not_a_limit_order -> "only limit orders can rest on the book"
  | Duplicate_order_id id ->
      Printf.sprintf "an order with id %s already rests" (Order_id.pp id)

let string_of_cancel_error = function
  | Unknown_order_id id ->
      Printf.sprintf "no resting order has id %s" (Order_id.pp id)

(* The (side, price) at which a stored order rests. Only [Limit] orders are ever
   placed in the book, so the other arms are unreachable by construction. *)
let resting_key = function
  | Order.Limit { side; price; _ } -> (side, price)
  | Order.Market _ | Order.Cancel _ ->
      invalid_arg "Order_book: non-limit order found resting (should be impossible)"

let side_map book = function Buy -> book.bids | Sell -> book.asks

let set_side_map book side side_map =
  match side with Buy -> { book with bids = side_map } | Sell -> { book with asks = side_map }

(* Priority comparison of two resting entries at the same price: earlier
   timestamp first, and for equal timestamps the earlier arrival (smaller
   [seq]). [seq] is unique across the book, so this never returns 0 for two
   distinct entries — the ordering is a strict total order. *)
let by_priority e1 e2 =
  match Timestamp.compare (Order.timestamp e1.order) (Order.timestamp e2.order) with
  | 0 -> Int.compare e1.seq e2.seq
  | c -> c

(* Insert [entry] into a price level kept in ascending priority order. A freshly
   added order carries the largest [seq] so far, so among orders of equal
   timestamp it lands {e after} those already resting — i.e. in arrival order —
   while a lower timestamp lets it jump ahead. Timestamps therefore need not
   arrive sorted: each order finds its correct time-priority slot on insertion. *)
let rec insert_by_priority entry = function
  | [] -> [ entry ]
  | x :: _ as level when by_priority entry x < 0 -> entry :: level
  | x :: rest -> x :: insert_by_priority entry rest

(** [add book order] rests [order] on its side, or returns an error.

    Only [Limit] orders rest: a [Market] or [Cancel] passed here is rejected with
    [Not_a_limit_order] and never enters the book. Re-using an id that already
    rests is rejected with [Duplicate_order_id]. The order is inserted into its
    price level at its time-priority position (earliest timestamp first; arrival
    order among equal timestamps), so it need not have arrived in timestamp
    order. *)
let add book (order : Order.t) : (t, add_error) result =
  match order with
  | Order.Market _ | Order.Cancel _ -> Error Not_a_limit_order
  | Order.Limit { id; side; price; _ } ->
      if Id_map.mem id book.index then Error (Duplicate_order_id id)
      else
        let entry = { order; seq = book.next_seq } in
        let map = side_map book side in
        let level = Option.value ~default:[] (Price_map.find_opt price map) in
        let map = Price_map.add price (insert_by_priority entry level) map in
        let book = set_side_map book side map in
        Ok
          {
            book with
            index = Id_map.add id entry book.index;
            next_seq = book.next_seq + 1;
          }

(** [cancel book id] removes the single resting order with this [id], cleaning up
    its price level if it becomes empty. Returns [Unknown_order_id] if no resting
    order has that id (cancel requests themselves never rest). *)
let cancel book id : (t, cancel_error) result =
  match Id_map.find_opt id book.index with
  | None -> Error (Unknown_order_id id)
  | Some entry ->
      let side, price = resting_key entry.order in
      let map = side_map book side in
      let level = Option.value ~default:[] (Price_map.find_opt price map) in
      (* ids are unique across the book, so this removes exactly one order. *)
      let level =
        List.filter (fun e -> not (Order_id.equal (Order.id e.order) id)) level
      in
      let map =
        match level with
        | [] -> Price_map.remove price map (* drop the now-empty level *)
        | _ -> Price_map.add price level map
      in
      let book = set_side_map book side map in
      Ok { book with index = Id_map.remove id book.index }

(** [reduce book id by] decreases the resting order [id]'s quantity by [by]
    units while preserving its [(timestamp, seq)] priority — and therefore its
    place in its price level. If [by] equals the whole resting quantity the order
    is removed and an emptied level pruned, exactly as {!cancel} would;
    otherwise the resting entry is replaced in place by a copy carrying
    [resting - by] units (still positive, so a valid [Quantity.t]) with the same
    timestamp and the same arrival [seq].

    This is the primitive the matching engine uses to consume liquidity.
    [cancel] followed by [add] cannot substitute for it: [add] stamps a fresh,
    larger [seq], which would demote a partially-filled resting order behind any
    same-timestamp orders resting at its price, violating price-time priority.

    Raises [Invalid_argument] if no resting order has [id], or if [by] exceeds
    the resting quantity. Both are caller-side invariant violations (the engine
    only ever reduces an order it just read, by at most its available quantity),
    handled the same way as the other "impossible" states in this module. *)
let reduce book id (by : Quantity.t) : t =
  match Id_map.find_opt id book.index with
  | None -> invalid_arg "Order_book.reduce: unknown order id"
  | Some { order = Order.Market _ | Order.Cancel _; _ } ->
      invalid_arg "Order_book.reduce: non-limit order found resting (should be impossible)"
  | Some ({ order = Order.Limit { side; price; quantity = resting_qty; timestamp; _ }; _ } as entry)
    -> (
      match Quantity.compare by resting_qty with
      | c when c > 0 -> invalid_arg "Order_book.reduce: reduction exceeds resting quantity"
      | 0 -> ( match cancel book id with Ok b -> b | Error _ -> assert false)
      | _ ->
          let new_qty = Quantity.to_int resting_qty - Quantity.to_int by in
          let new_order =
            match
              Order.limit ~id ~side ~price:(Price_ticks.to_int price) ~quantity:new_qty
                ~timestamp
            with
            | Ok o -> o
            | Error _ -> assert false (* new_qty > 0 and price was already valid *)
          in
          (* Same seq and timestamp, so the reduced order keeps its priority slot. *)
          let new_entry = { entry with order = new_order } in
          let map = side_map book side in
          let level = Option.value ~default:[] (Price_map.find_opt price map) in
          (* replace the order in its slot, leaving every other position intact *)
          let level =
            List.map
              (fun e -> if Order_id.equal (Order.id e.order) id then new_entry else e)
              level
          in
          let map = Price_map.add price level map in
          let book = set_side_map book side map in
          { book with index = Id_map.add id new_entry book.index })

(** [best_bid book] is the highest-priced resting bid, and among those the one
    with time priority (earliest timestamp, then earliest arrival), or [None] if
    there are no bids. *)
let best_bid book =
  match Price_map.max_binding_opt book.bids with
  | Some (_price, entry :: _) -> Some entry.order
  | Some (_, []) | None -> None

(** [best_ask book] is the lowest-priced resting ask, with time priority within
    the level, or [None] if there are no asks. *)
let best_ask book =
  match Price_map.min_binding_opt book.asks with
  | Some (_price, entry :: _) -> Some entry.order
  | Some (_, []) | None -> None

(** [reduce_best_bid book by] consumes [by] units from the current best bid via
    {!reduce}, so the reduced order keeps its time-priority slot. Raises
    [Invalid_argument] if there is no resting bid. This is the liquidity-consuming
    primitive the matching engine uses when a sell hits the book; exposing it
    (rather than [reduce] by arbitrary id) keeps the id-keyed mutation and its
    invariants internal. *)
let reduce_best_bid book (by : Quantity.t) : t =
  match best_bid book with
  | None -> invalid_arg "Order_book.reduce_best_bid: no resting bid"
  | Some order -> reduce book (Order.id order) by

(** [reduce_best_ask book by] is the ask-side counterpart, used when a buy lifts
    the book. Raises [Invalid_argument] if there is no resting ask. *)
let reduce_best_ask book (by : Quantity.t) : t =
  match best_ask book with
  | None -> invalid_arg "Order_book.reduce_best_ask: no resting ask"
  | Some order -> reduce book (Order.id order) by

(** [find book id] is the resting order with this id, or [None]. *)
let find book id = Option.map (fun e -> e.order) (Id_map.find_opt id book.index)

(** A debug/test view of the whole book: each side as a list of price levels in
    priority order (bids highest-first, asks lowest-first). Prices are unwrapped
    to plain ints for easy assertions; each level's order list is in time
    priority (earliest timestamp first; arrival order among equal timestamps).
    An empty level never appears (they are pruned on cancel). *)
type snapshot = {
  bid_levels : (int * Order.t list) list;
  ask_levels : (int * Order.t list) list;
}

let snapshot book =
  let level_view (price, entries) =
    (Price_ticks.to_int price, List.map (fun e -> e.order) entries)
  in
  {
    (* [bindings] is ascending by price; bids want highest-first, so reverse. *)
    bid_levels = List.rev_map level_view (Price_map.bindings book.bids);
    ask_levels = List.map level_view (Price_map.bindings book.asks);
  }

(** All resting bids flattened into full priority order (best first, time
    priority within a level). *)
let bids book = List.concat_map snd (snapshot book).bid_levels

(** All resting asks flattened into full priority order. *)
let asks book = List.concat_map snd (snapshot book).ask_levels

(** A human-readable dump of the book, best prices at the top of each side. *)
let to_string book =
  let order_str = function
    | Order.Limit { id; quantity; timestamp; _ } ->
        Printf.sprintf "%s x%s @%s" (Order_id.pp id) (Quantity.pp quantity)
          (Timestamp.pp timestamp)
    | Order.Market _ | Order.Cancel _ -> "<non-resting>"
  in
  let level_str (price, orders) =
    Printf.sprintf "  %d: %s" price (String.concat ", " (List.map order_str orders))
  in
  let s = snapshot book in
  let section title levels =
    title :: (if levels = [] then [ "  (empty)" ] else List.map level_str levels)
  in
  String.concat "\n"
    (section "ASKS (low->high):" (List.rev s.ask_levels)
    @ section "BIDS (high->low):" s.bid_levels)
