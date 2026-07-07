(** Metrics collection.

    A pure accumulator of counters over a single deterministic replay run.
    {!empty} is the zero and each [record_*]/[count_event] returns a fresh {!t}
    with the relevant counters advanced, so two runs over identical input produce
    structurally equal values. The counter fields are exposed so {!Replay} and
    tests can read them; the accumulator advances them only through the functions
    below. *)

(** A snapshot of run counters. All fields are independent tallies. *)
type t = {
  events_processed : int;
  accepted : int;
  rejected : int;
  fills : int;
  traded_quantity : int;
  filled : int;
  rested : int;
  discarded : int;
  cancelled : int;
  cancel_miss : int;
}

(** All counters at zero. *)
val empty : t

(** [count_event t] tallies one more processed event, regardless of verdict. *)
val count_event : t -> t

(** [record_rejected t] records that an order was rejected by the risk gate. *)
val record_rejected : t -> t

(** [record_accepted t result] folds an accepted order's matching [result] into
    the counters: bumps [accepted], adds its fills and traded volume, and
    advances exactly one outcome counter. *)
val record_accepted : t -> Matching_engine.result -> t

(** A stable, human-readable dump with a fixed field set and order. *)
val to_string : t -> string
