(** Deterministic replay.

    Reads a recorded stream of events and drives them through the pre-trade
    {!Risk_engine} and the {!Matching_engine} reproducibly: a given input always
    yields identical fills, positions, book state, and {!Metrics}. There is no
    wall-clock time and no randomness.

    Parsing helpers, the owner map that attributes fills to accounts, and the
    per-event driver are all internal; the public surface is parse, run, and the
    renderings around them. *)

open Types

(** Replay configuration: the risk limits every account is checked against, plus
    the fallback reference price used to value a market order when the opposing
    side of the book is empty. *)
type config = {
  limits : Risk_engine.Limits.t;
  default_reference_price : Price_ticks.t;
}

(** A generous default suitable for the checked-in example. *)
val default_config : config

(** A single parsed event: the order and the account that submitted it. *)
type event = {
  account : Risk_engine.Account_id.t;
  order : Order.t;
}

(** A line that could not be parsed (or that failed validation), with enough
    context to report it. *)
type parse_error = {
  line_number : int;
  text : string;
  message : string;
}

val string_of_parse_error : parse_error -> string

(** [parse contents] splits a whole file into lines and parses each, returning
    the events in order alongside any parse/validation errors (also in order).
    Order ids must be unique across the whole stream; the first occurrence is
    kept and later reuses are reported and dropped. *)
val parse : string -> event list * parse_error list

(** [read_file path] reads the whole file at [path]. Raises [Sys_error] on IO
    failure. *)
val read_file : string -> string

(** The outcome of a replay run. [positions] lists every account seen with its
    final net position, sorted ascending by account id for deterministic
    output. *)
type summary = {
  metrics : Metrics.t;
  book : Order_book.t;
  positions : (Risk_engine.Account_id.t * int) list;
}

(** [run config events] drives [events] through the engines in order and returns
    the final metrics, book, and per-account positions. *)
val run : config -> event list -> summary

(** [position summary account] is the final net position recorded for [account]
    (0 if the account never appeared). *)
val position : summary -> Risk_engine.Account_id.t -> int

(** A stable, human-readable rendering of a run summary. Identical for identical
    runs. *)
val summary_to_string : summary -> string
