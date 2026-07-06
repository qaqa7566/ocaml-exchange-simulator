(** Orders submitted to the exchange.

    An order is modelled as a variant whose arms each carry {e exactly} the
    fields that kind of order needs. This makes whole classes of invalid state
    unrepresentable rather than merely discouraged:

    - [Limit] always carries a [Price_ticks.t] (which is always positive by
      construction), so a limit order can never be missing a price or hold a
      nonpositive one.
    - [Market] carries no price field at all, so a market order can never carry
      a spurious price.
    - [Cancel] carries only the id of the order to remove — side, price, and
      quantity simply do not exist for it.

    The smart constructors below take raw integers for price/quantity and
    validate them through the {!Types} constructors, returning a [result] that
    names the reason on failure. *)

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

let string_of_error = function
  | Nonpositive_quantity n ->
      Printf.sprintf "quantity must be positive, got %d" n
  | Nonpositive_price n ->
      Printf.sprintf "limit price must be positive, got %d" n
  | Self_cancel id ->
      Printf.sprintf "cancel cannot target its own id %s" (Order_id.pp id)

(** [limit ~id ~side ~price ~quantity ~timestamp] builds a resting/aggressive
    limit order. Rejects a nonpositive [quantity] or [price]. *)
let limit ~id ~side ~price ~quantity ~timestamp : (t, error) result =
  match Quantity.create quantity with
  | None -> Error (Nonpositive_quantity quantity)
  | Some quantity -> (
      match Price_ticks.create price with
      | None -> Error (Nonpositive_price price)
      | Some price -> Ok (Limit { id; side; price; quantity; timestamp }))

(** [market ~id ~side ~quantity ~timestamp] builds a market order (no price).
    Rejects a nonpositive [quantity]. *)
let market ~id ~side ~quantity ~timestamp : (t, error) result =
  match Quantity.create quantity with
  | None -> Error (Nonpositive_quantity quantity)
  | Some quantity -> Ok (Market { id; side; quantity; timestamp })

(** [cancel ~id ~target ~timestamp] requests removal of the order [target].
    Rejects a cancel that targets its own id, the one structurally invalid
    cancel. *)
let cancel ~id ~target ~timestamp : (t, error) result =
  if Order_id.equal id target then Error (Self_cancel target)
  else Ok (Cancel { id; target; timestamp })

(** The id of the submitted order itself (not the cancel target). *)
let id = function
  | Limit { id; _ } | Market { id; _ } | Cancel { id; _ } -> id

let timestamp = function
  | Limit { timestamp; _ } | Market { timestamp; _ } | Cancel { timestamp; _ } ->
      timestamp
