(** The pre-trade risk engine.

    Evaluates an incoming order against configured risk limits {e before} it
    reaches the matching engine. {!check} is a pure predicate over the order and
    the current risk state; the actual position change is applied later, from
    real fills, via {!apply_fill}. The risk state {!t} is abstract — accounts and
    their positions/kill switches are read and updated only through the functions
    below. *)

open Types

(** A typed account / user identifier, abstract so it cannot be confused with
    another kind of label. *)
module Account_id : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> string
end

(** The risk limits applied to every account. All caps are inclusive. *)
module Limits : sig
  type t

  (** [create ~max_order_quantity ~max_position ~max_notional] builds a limit
      set; all three caps are inclusive upper bounds and expected nonnegative. *)
  val create :
    max_order_quantity:int -> max_position:int -> max_notional:int -> t

  val max_order_quantity : t -> int
  val max_position : t -> int
  val max_notional : t -> int
end

(** Immutable risk state: shared limits plus per-account positions and kill
    switches. Every operation returns a fresh value. *)
type t

(** [empty limits] is risk state with the given limits, every account flat and
    no kill switches engaged. *)
val empty : Limits.t -> t

val limits : t -> Limits.t

(** [position t account] is the account's current net signed position (long
    positive, short negative), or 0 if the account is unknown. *)
val position : t -> Account_id.t -> int

(** [is_kill_switch t account] is whether the account's kill switch is engaged. *)
val is_kill_switch : t -> Account_id.t -> bool

(** [set_kill_switch t account on] returns risk state with the account's kill
    switch set to [on], leaving everything else untouched. *)
val set_kill_switch : t -> Account_id.t -> bool -> t

(** Why an order was rejected; each reason carries the numbers that justify it. *)
type rejection =
  | Kill_switch_engaged of Account_id.t
  | Order_quantity_exceeded of { limit : int; requested : int }
  | Position_limit_exceeded of { limit : int; projected : int }
  | Notional_limit_exceeded of { limit : int; projected : int }
  | No_reference_price
      (** a market order was checked without a reference price to value it *)

(** The verdict of a pre-trade check. *)
type decision =
  | Accepted
  | Rejected of rejection

val string_of_rejection : rejection -> string
val string_of_decision : decision -> string

(** [check t ~account ?reference_price order] decides whether [order], attributed
    to [account], may be sent to the matching engine. Pure: [t] is never
    modified. [reference_price] is required for a {!Order.Market} and an optional
    conservative override for a {!Order.Limit}. *)
val check :
  t ->
  account:Account_id.t ->
  ?reference_price:Price_ticks.t ->
  Order.t ->
  decision

(** [apply_fill t ~account ~side quantity] returns risk state with [account]'s
    position moved by an executed fill: a buy adds [quantity], a sell subtracts
    it. This is the only way positions change. *)
val apply_fill : t -> account:Account_id.t -> side:side -> Quantity.t -> t
