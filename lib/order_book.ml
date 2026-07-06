(** The limit order book (resting orders only; no matching).

    Maintains resting bids and asks under price-time priority: best price first,
    and within a price level, earliest arrival first (FIFO). This module is the
    passive container — it stores, indexes, and removes resting limit orders. It
    performs {e no} matching: an added order always rests, even if it would cross
    the book. Crossing is resolved later by {!Matching_engine}, so this module
    intentionally has no "no crossed book" assertion.

    {2 Representation}

    The book is immutable: every mutating operation returns a new {!t}. This
    mirrors the functional style of the Phase 1 types, makes a book value a
    cheap, first-class snapshot (useful for deterministic replay and tests), and
    rules out aliasing bugs. The cost is allocation per update (the price maps
    rebalance and the FIFO list is copied on append). A future matching hot loop
    may cache best prices and switch to a mutable level structure behind this
    same interface; the public signatures here are meant to survive that. *)

open Types

(* Price levels are kept in a map keyed by price. Bids and asks share the same
   ascending key ordering; the "best" end differs per side (highest bid, lowest
   ask) and is read via [max_binding]/[min_binding] respectively. *)
module Price_map = Map.Make (Price_ticks)

(* Secondary index from order id to the resting order, for O(log N) lookup and
   cancellation without scanning price levels. Only [Limit] orders ever enter
   it. *)
module Id_map = Map.Make (Order_id)

type t = {
  bids : Order.t list Price_map.t;
      (** price -> orders at that price, oldest first (FIFO) *)
  asks : Order.t list Price_map.t;
  index : Order.t Id_map.t;  (** id -> the resting order it refers to *)
}

let empty = { bids = Price_map.empty; asks = Price_map.empty; index = Id_map.empty }

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

(** [add book order] rests [order] on its side, or returns an error.

    Only [Limit] orders rest: a [Market] or [Cancel] passed here is rejected with
    [Not_a_limit_order] and never enters the book. Re-using an id that already
    rests is rejected with [Duplicate_order_id]. The order is appended to the
    tail of its price level, preserving FIFO/time priority. *)
let add book (order : Order.t) : (t, add_error) result =
  match order with
  | Order.Market _ | Order.Cancel _ -> Error Not_a_limit_order
  | Order.Limit { id; side; price; _ } ->
      if Id_map.mem id book.index then Error (Duplicate_order_id id)
      else
        let map = side_map book side in
        let level = Option.value ~default:[] (Price_map.find_opt price map) in
        let map = Price_map.add price (level @ [ order ]) map in
        let book = set_side_map book side map in
        Ok { book with index = Id_map.add id order book.index }

(** [cancel book id] removes the single resting order with this [id], cleaning up
    its price level if it becomes empty. Returns [Unknown_order_id] if no resting
    order has that id (cancel requests themselves never rest). *)
let cancel book id : (t, cancel_error) result =
  match Id_map.find_opt id book.index with
  | None -> Error (Unknown_order_id id)
  | Some order ->
      let side, price = resting_key order in
      let map = side_map book side in
      let level = Option.value ~default:[] (Price_map.find_opt price map) in
      (* ids are unique across the book, so this removes exactly one order. *)
      let level =
        List.filter (fun o -> not (Order_id.equal (Order.id o) id)) level
      in
      let map =
        match level with
        | [] -> Price_map.remove price map (* drop the now-empty level *)
        | _ -> Price_map.add price level map
      in
      let book = set_side_map book side map in
      Ok { book with index = Id_map.remove id book.index }

(** [best_bid book] is the highest-priced resting bid, and among those the
    earliest to arrive, or [None] if there are no bids. *)
let best_bid book =
  match Price_map.max_binding_opt book.bids with
  | Some (_price, order :: _) -> Some order
  | Some (_price, []) -> None
  | None -> None

(** [best_ask book] is the lowest-priced resting ask, earliest-arrival first, or
    [None] if there are no asks. *)
let best_ask book =
  match Price_map.min_binding_opt book.asks with
  | Some (_price, order :: _) -> Some order
  | Some (_price, []) -> None
  | None -> None

(** [find book id] is the resting order with this id, or [None]. *)
let find book id = Id_map.find_opt id book.index

(** A debug/test view of the whole book: each side as a list of price levels in
    priority order (bids highest-first, asks lowest-first). Prices are unwrapped
    to plain ints for easy assertions; each level's order list is oldest-first
    (FIFO). An empty level never appears (they are pruned on cancel). *)
type snapshot = {
  bid_levels : (int * Order.t list) list;
  ask_levels : (int * Order.t list) list;
}

let snapshot book =
  let level_view (price, orders) = (Price_ticks.to_int price, orders) in
  {
    (* [bindings] is ascending by price; bids want highest-first, so reverse. *)
    bid_levels = List.rev_map level_view (Price_map.bindings book.bids);
    ask_levels = List.map level_view (Price_map.bindings book.asks);
  }

(** All resting bids flattened into full priority order (best first, FIFO within
    a level). *)
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
