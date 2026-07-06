(** The limit order book.

    Maintains resting bids and asks sorted by price-time priority: best price
    first, and within a price level, earliest arrival first (FIFO). Not yet
    implemented. *)

(* TODO: implement the book representation (price levels, FIFO queues), plus
   insert / remove / best_bid / best_ask operations. *)
