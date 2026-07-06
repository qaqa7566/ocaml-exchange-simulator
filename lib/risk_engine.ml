(** The risk engine.

    Evaluates orders against configured risk limits (e.g. position, notional,
    and order-size caps) before they reach the matching engine, rejecting those
    that would breach a limit. Not yet implemented. *)

(* TODO: define risk limit configuration and the pre-trade check that accepts
   or rejects an order given current positions. *)
