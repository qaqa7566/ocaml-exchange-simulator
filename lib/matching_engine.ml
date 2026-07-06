(** The matching engine.

    Consumes incoming orders and matches them against the resting book under
    price-time priority, producing fills (including partial fills) and resting
    remainders. Not yet implemented. *)

(* TODO: implement matching for market, limit, and cancel orders, emitting
   fills and updating the order book while preserving FIFO at each price
   level. *)
