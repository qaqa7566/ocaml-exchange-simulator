(** Metrics collection.

    Aggregates counters over a single deterministic replay run: how many events
    were processed, how the risk gate and matching engine classified each order,
    and how much volume actually traded. Every field is a plain integer counter,
    so two runs over identical input produce structurally equal {!t} values —
    the basis of the determinism guarantee that {!Replay} exposes.

    The module is a pure accumulator: {!empty} is the zero, and each [record_*]
    function returns a fresh {!t} with the relevant counters advanced. It knows
    about {!Matching_engine} only to read an order's outcome and fills; it never
    drives the engine itself. *)

open Types
module ME = Matching_engine

(** A snapshot of run counters. All fields are independent tallies:

    - [events_processed] — parsed events fed through the pipeline (accepted or
      not); parse errors are reported separately by {!Replay} and not counted.
    - [accepted] / [rejected] — the pre-trade risk verdict for each order.
    - [fills] — individual executions produced by the matching engine.
    - [traded_quantity] — total units that changed hands across all fills.
    - [filled] / [rested] / [discarded] / [cancelled] / [cancel_miss] — the
      matching {!Matching_engine.outcome} of each {e accepted} order. These
      partition the accepted count that produced an outcome (a cancel that hits
      counts as [cancelled], one that misses as [cancel_miss]). *)
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
let empty =
  {
    events_processed = 0;
    accepted = 0;
    rejected = 0;
    fills = 0;
    traded_quantity = 0;
    filled = 0;
    rested = 0;
    discarded = 0;
    cancelled = 0;
    cancel_miss = 0;
  }

(** [count_event t] tallies one more processed event. Called once per event
    regardless of the risk verdict. *)
let count_event t = { t with events_processed = t.events_processed + 1 }

(** [record_rejected t] records that an order was rejected by the risk gate. *)
let record_rejected t = { t with rejected = t.rejected + 1 }

(** [record_accepted t result] folds an accepted order's matching [result] into
    the counters: it bumps [accepted], adds this order's fills and traded volume,
    and advances exactly one outcome counter. *)
let record_accepted t (result : ME.result) =
  let n_fills = List.length result.ME.fills in
  let traded =
    List.fold_left
      (fun acc (f : ME.fill) -> acc + Quantity.to_int f.ME.quantity)
      0 result.ME.fills
  in
  let t =
    {
      t with
      accepted = t.accepted + 1;
      fills = t.fills + n_fills;
      traded_quantity = t.traded_quantity + traded;
    }
  in
  match result.ME.outcome with
  | ME.Filled -> { t with filled = t.filled + 1 }
  | ME.Rested _ -> { t with rested = t.rested + 1 }
  | ME.Discarded _ -> { t with discarded = t.discarded + 1 }
  | ME.Cancelled _ -> { t with cancelled = t.cancelled + 1 }
  | ME.Cancel_miss _ -> { t with cancel_miss = t.cancel_miss + 1 }

(** A stable, human-readable dump. The field set and order are fixed so the
    output is identical for identical runs. *)
let to_string t =
  String.concat "\n"
    [
      Printf.sprintf "events processed : %d" t.events_processed;
      Printf.sprintf "accepted         : %d" t.accepted;
      Printf.sprintf "rejected         : %d" t.rejected;
      Printf.sprintf "fills            : %d" t.fills;
      Printf.sprintf "traded quantity  : %d" t.traded_quantity;
      Printf.sprintf "filled orders    : %d" t.filled;
      Printf.sprintf "rested orders    : %d" t.rested;
      Printf.sprintf "discarded orders : %d" t.discarded;
      Printf.sprintf "cancelled        : %d" t.cancelled;
      Printf.sprintf "cancel misses    : %d" t.cancel_miss;
    ]
