(** The pre-trade risk engine.

    Evaluates an incoming order against configured risk limits {e before} it
    reaches the matching engine, and rejects any order that would breach a
    limit. This is a pure gate: {!check} inspects the order and the current risk
    state and returns a {!decision}; it never mutates state.

    {2 What is checked}

    - {b Max order quantity} — a single order may not exceed a size cap.
    - {b Max absolute position} — the account's net position, after the order
      fully fills, may not exceed a cap in either direction.
    - {b Max notional exposure} — the account's projected net position, valued
      at a reference price, may not exceed a notional cap.
    - {b Kill switch} — an account may be flagged so that no new risk-increasing
      order is admitted.

    {2 Position-sign convention}

    A position is a single signed integer of net units. A {e buy} adds to it
    (+quantity), a {e sell} subtracts from it (-quantity). A positive position is
    long, a negative position is short, zero is flat. The max-absolute-position
    cap is therefore one check on [abs projected], covering both a long breach
    (too positive) and a short breach (too negative).

    {2 Projected, worst-case evaluation}

    The check assumes the {e whole} order quantity fills — the worst case for
    risk — and evaluates the resulting [projected = current + signed order]. An
    order may in reality fill only partially, or (a limit) rest unfilled, so the
    order's true effect on the position is known only after matching. That is why
    the pre-trade check is kept separate from position updates: {!check} is a
    pure predicate, while the {e actual} position change is applied later, from
    real fills, via {!apply_fill}.

    {2 Reference price for notional}

    A limit order carries its own price, used to value the position. A market
    order has no price, so the caller must supply a conservative
    [~reference_price] (e.g. a worst-case bound on the fill price). Without one,
    a market order is rejected with {!No_reference_price} rather than valued at
    an assumed price. A caller may also pass [~reference_price] for a limit order
    to value it more conservatively than its limit price. *)

open Types

(** A typed account / user identifier.

    Abstract, like {!Types.Order_id}, so it cannot be confused with another kind
    of label. It carries no invariant — an account id is just a name — and its
    {!compare} lets the engine key per-account state in a map. *)
module Account_id : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> string
end = struct
  type t = string

  let of_string s = s
  let to_string s = s
  let equal = String.equal
  let compare = String.compare
  let pp s = s
end

module Account_map = Map.Make (Account_id)

(** The risk limits applied to every account.

    Limits are global configuration: each account is checked independently
    against the same caps, while positions and kill switches are tracked per
    account. All caps are inclusive — a value {e exactly at} a cap is accepted;
    only a value strictly beyond it is rejected. *)
module Limits : sig
  type t

  (** [create ~max_order_quantity ~max_position ~max_notional] builds a limit
      set. All three caps are inclusive upper bounds; they are expected to be
      nonnegative. *)
  val create :
    max_order_quantity:int -> max_position:int -> max_notional:int -> t

  val max_order_quantity : t -> int
  val max_position : t -> int
  val max_notional : t -> int
end = struct
  type t = {
    max_order_quantity : int;
    max_position : int;
    max_notional : int;
  }

  let create ~max_order_quantity ~max_position ~max_notional =
    { max_order_quantity; max_position; max_notional }

  let max_order_quantity t = t.max_order_quantity
  let max_position t = t.max_position
  let max_notional t = t.max_notional
end

(** Per-account mutable-looking but immutably-stored state: the net signed
    position and whether the kill switch is engaged. *)
type account_state = { position : int; kill_switch : bool }

let default_state = { position = 0; kill_switch = false }

(** Immutable risk state: the shared limits plus per-account positions and kill
    switches. Every operation returns a fresh value; nothing is mutated. *)
type t = { limits : Limits.t; accounts : account_state Account_map.t }

(** [empty limits] is risk state with the given limits, every account flat
    (position 0) and no kill switches engaged. *)
let empty limits = { limits; accounts = Account_map.empty }

let limits t = t.limits

let state_of t account =
  match Account_map.find_opt account t.accounts with
  | Some s -> s
  | None -> default_state

(** [position t account] is the account's current net signed position (long
    positive, short negative), or 0 if the account is unknown. *)
let position t account = (state_of t account).position

(** [is_kill_switch t account] is whether the account's kill switch is engaged. *)
let is_kill_switch t account = (state_of t account).kill_switch

(** [set_kill_switch t account on] returns risk state with the account's kill
    switch set to [on], leaving every other account and all positions untouched. *)
let set_kill_switch t account on =
  let s = state_of t account in
  { t with accounts = Account_map.add account { s with kill_switch = on } t.accounts }

(** Why an order was rejected. Each reason carries the numbers that justify it —
    the offending [requested]/[projected] value alongside the [limit] it broke —
    so the caller can report or log a precise cause. [projected] is signed, so a
    position breach reveals whether it was long (positive) or short (negative). *)
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

let string_of_rejection = function
  | Kill_switch_engaged a ->
      Printf.sprintf "kill switch engaged for account %s" (Account_id.pp a)
  | Order_quantity_exceeded { limit; requested } ->
      Printf.sprintf "order quantity %d exceeds max %d" requested limit
  | Position_limit_exceeded { limit; projected } ->
      Printf.sprintf "projected position %d exceeds max abs %d" projected limit
  | Notional_limit_exceeded { limit; projected } ->
      Printf.sprintf "projected position %d breaches max notional %d" projected
        limit
  | No_reference_price ->
      "market order has no price and no reference price was supplied"

let string_of_decision = function
  | Accepted -> "accepted"
  | Rejected r -> "rejected: " ^ string_of_rejection r

(* The signed direction a side moves a position: a buy adds units, a sell
   removes them. *)
let signed_quantity side qty =
  match side with Buy -> qty | Sell -> -qty

(* The price at which to value the position for the notional check. A limit order
   uses its own price unless the caller supplies a (conservative) override; a
   market order has no price and must be given one. *)
let valuation_price (order : Order.t) ~reference_price =
  match (order, reference_price) with
  | _, Some p -> Ok (Price_ticks.to_int p)
  | Order.Limit { price; _ }, None -> Ok (Price_ticks.to_int price)
  | Order.Market _, None -> Error No_reference_price
  | Order.Cancel _, None -> Ok 0 (* cancels are never valued; see [check] *)

(** [check t ~account ?reference_price order] decides whether [order], attributed
    to [account], may be sent to the matching engine given the current risk
    state [t]. It is a pure predicate: [t] is never modified.

    A {!Order.Cancel} is always accepted — it can only reduce exposure. Otherwise
    the checks are applied in order of severity and the first breach is returned:
    kill switch, then max order quantity, then max absolute position, then max
    notional. Every cap is inclusive, so an order landing exactly on a limit is
    accepted.

    [reference_price] is required for a {!Order.Market} (which has no price of its
    own); for a {!Order.Limit} it is an optional conservative override of the
    limit price. *)
let check t ~account ?reference_price (order : Order.t) : decision =
  match order with
  | Order.Cancel _ -> Accepted (* risk-reducing: always admitted *)
  | Order.Limit { side; quantity; _ } | Order.Market { side; quantity; _ } ->
      let limits = t.limits in
      let s = state_of t account in
      let requested = Quantity.to_int quantity in
      if s.kill_switch then Rejected (Kill_switch_engaged account)
      else if requested > Limits.max_order_quantity limits then
        Rejected
          (Order_quantity_exceeded
             { limit = Limits.max_order_quantity limits; requested })
      else
        let projected = s.position + signed_quantity side requested in
        if abs projected > Limits.max_position limits then
          Rejected
            (Position_limit_exceeded
               { limit = Limits.max_position limits; projected })
        else
          match valuation_price order ~reference_price with
          | Error r -> Rejected r
          | Ok price ->
              let notional = abs projected * price in
              if notional > Limits.max_notional limits then
                Rejected
                  (Notional_limit_exceeded
                     { limit = Limits.max_notional limits; projected })
              else Accepted

(** [apply_fill t ~account ~side quantity] returns risk state with [account]'s
    position moved by an executed fill: a buy adds [quantity], a sell subtracts
    it. This is the post-matching counterpart to {!check} that Phase 5 uses to
    fold the matching engine's real fills into positions — one call per fill, per
    account, threading the returned state. It is the {e only} way positions
    change, keeping the pre-trade check free of side effects. *)
let apply_fill t ~account ~side quantity =
  let s = state_of t account in
  let position = s.position + signed_quantity side (Quantity.to_int quantity) in
  { t with accounts = Account_map.add account { s with position } t.accounts }
