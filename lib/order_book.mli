(** The limit order book (resting orders only; no matching).

    Maintains resting bids and asks under price-time priority: best price first,
    and within a price level the earliest timestamp first, arrival order breaking
    exact ties. The book is immutable — every mutating operation returns a new
    {!t}.

    The concrete representation (the resting entry records, the per-book arrival
    sequence numbers, and the price/id maps) is deliberately hidden: it is an
    implementation detail that a future mutable hot loop may replace behind this
    same interface. *)

open Types

type t

val empty : t

(** Why {!add} refused to rest an order. *)
type add_error =
  | Not_a_limit_order  (** a market or cancel order was passed to [add] *)
  | Duplicate_order_id of Order_id.t  (** an order with this id already rests *)

(** Why {!cancel} could not remove an order. *)
type cancel_error = Unknown_order_id of Order_id.t
    (** no resting order has this id *)

val string_of_add_error : add_error -> string
val string_of_cancel_error : cancel_error -> string

(** [add book order] rests [order] on its side at its time-priority position, or
    returns an error. Only [Limit] orders rest; a duplicate resting id is
    rejected. *)
val add : t -> Order.t -> (t, add_error) result

(** [cancel book id] removes the single resting order with this [id], pruning an
    emptied price level. *)
val cancel : t -> Order_id.t -> (t, cancel_error) result

(** [best_bid book] is the highest-priced resting bid (time priority within the
    level), or [None]. *)
val best_bid : t -> Order.t option

(** [best_ask book] is the lowest-priced resting ask (time priority within the
    level), or [None]. *)
val best_ask : t -> Order.t option

(** [reduce_best_bid book by] consumes [by] units from the current best bid,
    removing it if fully consumed and otherwise shrinking it in place while
    preserving its time-priority slot. This is the liquidity-consumption
    primitive the matching engine uses when a sell hits the book. Raises
    [Invalid_argument] if there is no resting bid or [by] exceeds the best bid's
    resting quantity — both caller-side invariant violations (the engine only
    reduces the best order it just read, by at most its available quantity). *)
val reduce_best_bid : t -> Quantity.t -> t

(** [reduce_best_ask book by] is the ask-side counterpart of {!reduce_best_bid},
    used when a buy lifts the book. Same raising contract. *)
val reduce_best_ask : t -> Quantity.t -> t

(** [find book id] is the resting order with this id, or [None]. *)
val find : t -> Order_id.t -> Order.t option

(** A debug/test view of the whole book: each side as price levels in priority
    order (bids highest-first, asks lowest-first), prices unwrapped to ints and
    each level's orders in time priority. Empty levels never appear. *)
type snapshot = {
  bid_levels : (int * Order.t list) list;
  ask_levels : (int * Order.t list) list;
}

val snapshot : t -> snapshot

(** All resting bids flattened into full priority order (best first). *)
val bids : t -> Order.t list

(** All resting asks flattened into full priority order (best first). *)
val asks : t -> Order.t list

(** A human-readable dump of the book, best prices at the top of each side. *)
val to_string : t -> string
