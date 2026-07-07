(** Orders submitted to the exchange.

    An order is a variant whose arms each carry exactly the fields that kind of
    order needs, so whole classes of invalid state are unrepresentable. The
    variant is public because every consumer (book, matching engine, risk engine,
    replay, tests) pattern-matches on the order kind and its fields; the smart
    constructors below are the only sanctioned way to {e build} one, since they
    validate raw ints through the {!Types} constructors. *)

open Types

type t =
  | Limit of {
      id : Order_id.t;
      side : side;
      price : Price_ticks.t;
      quantity : Quantity.t;
      timestamp : Timestamp.t;
    }
  | Market of {
      id : Order_id.t;
      side : side;
      quantity : Quantity.t;
      timestamp : Timestamp.t;
    }
  | Cancel of {
      id : Order_id.t;
      target : Order_id.t;
      timestamp : Timestamp.t;
    }

(** Why a construction was rejected. *)
type error =
  | Nonpositive_quantity of int
  | Nonpositive_price of int
  | Self_cancel of Order_id.t

val string_of_error : error -> string

(** [limit ~id ~side ~price ~quantity ~timestamp] builds a limit order, rejecting
    a nonpositive [quantity] or [price]. *)
val limit :
  id:Order_id.t ->
  side:side ->
  price:int ->
  quantity:int ->
  timestamp:Timestamp.t ->
  (t, error) result

(** [market ~id ~side ~quantity ~timestamp] builds a market order (no price),
    rejecting a nonpositive [quantity]. *)
val market :
  id:Order_id.t ->
  side:side ->
  quantity:int ->
  timestamp:Timestamp.t ->
  (t, error) result

(** [cancel ~id ~target ~timestamp] requests removal of [target], rejecting a
    cancel that targets its own id. *)
val cancel :
  id:Order_id.t ->
  target:Order_id.t ->
  timestamp:Timestamp.t ->
  (t, error) result

(** The id of the submitted order itself (not the cancel target). *)
val id : t -> Order_id.t

val timestamp : t -> Timestamp.t
