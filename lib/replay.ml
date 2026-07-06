(** Deterministic replay.

    Reads a recorded stream of orders/events and feeds them through the risk
    and matching engines in a reproducible way, so that a given input stream
    always yields identical fills and book state. Not yet implemented. *)

(* TODO: implement loading an event stream (from data/) and driving the engines
   deterministically, with no reliance on wall-clock time or randomness. *)
