(** The matching engine.

    Matches incoming orders against the resting {!Order_book} under price-time
    priority, producing fills and resting remainders. Purely functional:
    {!process} takes a book and an order and returns a new book plus the fills
    and a typed {!outcome}; the input book is never mutated.

    The matching loop and its price-acceptability and liquidity-consumption
    helpers are internal — the whole contract is {!process} and the types it
    returns. *)

open Types

(** A single execution between an incoming (aggressor) order and one resting
    order. *)
type fill = {
  resting_id : Order_id.t;  (** the resting order that supplied liquidity *)
  incoming_id : Order_id.t;  (** the aggressor whose arrival caused the trade *)
  price : Price_ticks.t;  (** execution price = the resting order's price *)
  quantity : Quantity.t;  (** units traded in this fill, always [> 0] *)
  incoming_side : side;  (** side of the aggressor *)
}

(** What became of the incoming order once matching finished. *)
type outcome =
  | Filled  (** executed in full; nothing rested *)
  | Rested of Quantity.t  (** a limit left this many units resting *)
  | Discarded of Quantity.t  (** a market order's unfillable remainder, not rested *)
  | Cancelled of Order_id.t  (** a cancel removed this resting target *)
  | Cancel_miss of Order_id.t  (** a cancel named a target that was not resting *)

(** The full result of processing one order. *)
type result = {
  book : Order_book.t;  (** the book after the order was processed *)
  fills : fill list;  (** fills in execution order (best/earliest first) *)
  outcome : outcome;
}

val string_of_outcome : outcome -> string

(** [process book order] matches [order] against [book] and returns the updated
    book, the fills produced, and a typed {!outcome}. *)
val process : Order_book.t -> Order.t -> result
