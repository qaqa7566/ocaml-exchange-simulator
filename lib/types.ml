(** Core domain types shared across the exchange simulator.

    Prices and quantities use integers to keep matching deterministic and free
    of floating-point rounding error. Prices are expressed in ticks (the
    smallest price increment) and quantities in whole units.

    Each scalar is wrapped in its own module with an abstract type. This keeps
    the different kinds of integer distinct at the type level (a price can never
    be passed where a quantity is expected) and — for {!Quantity} and
    {!Price_ticks} — lets the smart constructor guarantee that every value in
    existence is strictly positive. The invariant lives in the type, so callers
    never have to re-check it. *)

(** Which side of the book an order rests on. *)
type side =
  | Buy
  | Sell

let string_of_side = function Buy -> "Buy" | Sell -> "Sell"

(** Monotonically increasing identifier assigned to every submitted order.

    Abstract so it cannot be confused with a price, quantity, or timestamp.
    Construction is total: an id is just a label, it carries no positivity
    invariant. *)
module Order_id : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> string
end = struct
  type t = int

  let of_int i = i
  let to_int i = i
  let equal = Int.equal
  let compare = Int.compare
  let pp i = Printf.sprintf "#%d" i
end

(** Logical timestamp used to enforce time priority. Replayed event streams
    provide these so that matching is fully deterministic.

    Abstract and total; time ordering is expressed through {!compare}. *)
module Timestamp : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> string
end = struct
  type t = int

  let of_int i = i
  let to_int i = i
  let equal = Int.equal
  let compare = Int.compare
  let pp i = Printf.sprintf "t%d" i
end

(** Quantity expressed in whole units.

    The only way to build a value is {!create}, which rejects nonpositive
    input. Every [Quantity.t] is therefore guaranteed [> 0]. *)
module Quantity : sig
  type t

  (** [create n] is [Some q] when [n > 0], otherwise [None]. *)
  val create : int -> t option

  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> string
end = struct
  type t = int

  let create n = if n > 0 then Some n else None
  let to_int n = n
  let equal = Int.equal
  let compare = Int.compare
  let pp n = string_of_int n
end

(** Price expressed in integer ticks (the smallest price increment).

    As with {!Quantity}, the only constructor is {!create}, which rejects
    nonpositive input, so every [Price_ticks.t] is guaranteed [> 0]. A price is
    only ever attached to a limit order, so this positivity guarantee is exactly
    what a valid limit order needs. *)
module Price_ticks : sig
  type t

  (** [create ticks] is [Some p] when [ticks > 0], otherwise [None]. *)
  val create : int -> t option

  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t -> string
end = struct
  type t = int

  let create ticks = if ticks > 0 then Some ticks else None
  let to_int t = t
  let equal = Int.equal
  let compare = Int.compare
  let pp t = string_of_int t
end
