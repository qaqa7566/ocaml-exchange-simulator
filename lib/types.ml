(** Core domain types shared across the exchange simulator.

    Prices and quantities use integers to keep matching deterministic and free
    of floating-point rounding error. Prices are expressed in ticks (the
    smallest price increment) and quantities in whole units. *)

(** Which side of the book an order rests on. *)
type side =
  | Buy
  | Sell

(** The kind of order being submitted.

    - [Market]: execute immediately against the best available prices.
    - [Limit]: execute only at [price] or better; the remainder rests on the
      book.
    - [Cancel]: remove a previously submitted order identified by its id. *)
type order_type =
  | Market
  | Limit
  | Cancel

(** Monotonically increasing identifier assigned to every submitted order. *)
type order_id = int

(** Price expressed in integer ticks. *)
type price = int

(** Quantity expressed in whole units. *)
type quantity = int

(** Logical timestamp used to enforce time priority. Replayed event streams
    provide these so that matching is fully deterministic. *)
type timestamp = int
