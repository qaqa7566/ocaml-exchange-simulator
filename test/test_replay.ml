(** Tests for deterministic replay.

    Dependency-free, in the same style as the other suites: each check is an
    [assert]-like predicate labelled so a failure aborts the run and names the
    offending case. Events are built from inline CSV text (exercising the real
    parser) and driven through {!Exchange.Replay.run}. Covers determinism, that
    a rejected order leaves book and positions untouched, partial-fill position
    accounting across two accounts, cancellation, market orders never resting,
    and exact final metric counts. *)

open Exchange
open Types

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("FAILED: " ^ name)

let acct = Risk_engine.Account_id.of_string

(* Parse inline CSV, asserting there are no parse errors, then run it. *)
let run_csv ?(config = Replay.default_config) csv =
  let events, errors = Replay.parse csv in
  (match errors with
  | [] -> ()
  | e :: _ -> failwith ("unexpected parse error: " ^ Replay.string_of_parse_error e));
  Replay.run config events

(* --- 1. Determinism: identical input -> identical output twice --------- *)

let () =
  let csv =
    "1,alice,1,sell,limit,101,10,\n\
     2,bob,2,buy,limit,101,4,\n\
     3,carol,3,buy,market,,3,\n\
     4,alice,4,sell,limit,105,5,\n\
     5,alice,5,,cancel,,,4"
  in
  let a = run_csv csv in
  let b = run_csv csv in
  check "determinism: summaries render identically"
    (Replay.summary_to_string a = Replay.summary_to_string b);
  check "determinism: metrics structurally equal" (a.Replay.metrics = b.Replay.metrics);
  check "determinism: positions equal" (a.Replay.positions = b.Replay.positions)

(* --- 2. Rejected order does not affect book or position ---------------- *)

let () =
  (* Tiny order-quantity cap so the second order is rejected outright. *)
  let config =
    {
      Replay.limits =
        Risk_engine.Limits.create ~max_order_quantity:5 ~max_position:1000
          ~max_notional:1_000_000;
      default_reference_price = Option.get (Price_ticks.create 100);
    }
  in
  let baseline = run_csv ~config "1,alice,1,sell,limit,101,4," in
  (* Same first order, then a second order that breaches the qty cap. *)
  let with_reject =
    run_csv ~config "1,alice,1,sell,limit,101,4,\n2,bob,2,buy,limit,101,50,"
  in
  check "reject: one rejection recorded" (with_reject.Replay.metrics.rejected = 1);
  check "reject: accepted count unchanged from baseline"
    (with_reject.Replay.metrics.accepted = baseline.Replay.metrics.accepted);
  check "reject: book identical to baseline (no effect)"
    (Order_book.to_string with_reject.Replay.book
    = Order_book.to_string baseline.Replay.book);
  check "reject: bob has no position" (Replay.position with_reject (acct "bob") = 0);
  check "reject: alice position unchanged (still resting, unfilled)"
    (Replay.position with_reject (acct "alice") = 0);
  check "reject: no fills" (with_reject.Replay.metrics.fills = 0)

(* --- 3. Partial fill updates both accounts' positions ------------------ *)

let () =
  (* alice rests a sell of 10; bob buys 4 at the same price -> 4 trade, 6 rest.
     Seller goes short 4, buyer long 4. *)
  let s = run_csv "1,alice,1,sell,limit,100,10,\n2,bob,2,buy,limit,100,4," in
  check "partial: one fill" (s.Replay.metrics.fills = 1);
  check "partial: traded quantity 4" (s.Replay.metrics.traded_quantity = 4);
  check "partial: seller short 4" (Replay.position s (acct "alice") = -4);
  check "partial: buyer long 4" (Replay.position s (acct "bob") = 4);
  check "partial: remainder of 6 rests on the ask"
    (match Order_book.best_ask s.Replay.book with
    | Some (Order.Limit { quantity; _ }) -> Quantity.to_int quantity = 6
    | _ -> false);
  check "partial: bob fully filled (nothing rests as a bid)"
    (Order_book.best_bid s.Replay.book = None)

(* --- 4. Cancellation via replay --------------------------------------- *)

let () =
  let s =
    run_csv "1,alice,1,buy,limit,100,5,\n2,alice,2,,cancel,,,1"
  in
  check "cancel: one cancellation recorded" (s.Replay.metrics.cancelled = 1);
  check "cancel: target no longer on the book"
    (Order_book.find s.Replay.book (Order_id.of_int 1) = None);
  check "cancel: book empty after cancel"
    (Order_book.bids s.Replay.book = [] && Order_book.asks s.Replay.book = []);
  check "cancel: no position change from a never-filled/cancelled order"
    (Replay.position s (acct "alice") = 0)

(* --- 5. Market order never rests -------------------------------------- *)

let () =
  (* Only 3 units of liquidity; a market buy of 8 fills 3 and discards 5. *)
  let s = run_csv "1,alice,1,sell,limit,100,3,\n2,bob,2,buy,market,,8," in
  check "market: one discard recorded" (s.Replay.metrics.discarded = 1);
  check "market: traded quantity 3" (s.Replay.metrics.traded_quantity = 3);
  check "market: market order id 2 did not rest"
    (Order_book.find s.Replay.book (Order_id.of_int 2) = None);
  check "market: no resting bid at all" (Order_book.best_bid s.Replay.book = None);
  check "market: ask fully consumed" (Order_book.best_ask s.Replay.book = None);
  check "market: buyer long only what filled" (Replay.position s (acct "bob") = 3);
  check "market: seller short only what filled" (Replay.position s (acct "alice") = -3)

(* --- 6. Final metrics match expected counts (the shipped example) ------ *)

let () =
  let csv = Replay.read_file "../data/example_events.csv" in
  let s = run_csv csv in
  let m = s.Replay.metrics in
  check "example: 9 events processed" (m.Metrics.events_processed = 9);
  check "example: 8 accepted" (m.Metrics.accepted = 8);
  check "example: 1 rejected" (m.Metrics.rejected = 1);
  check "example: 3 fills" (m.Metrics.fills = 3);
  check "example: 15 traded" (m.Metrics.traded_quantity = 15);
  check "example: 2 filled" (m.Metrics.filled = 2);
  check "example: 4 rested" (m.Metrics.rested = 4);
  check "example: 1 discarded" (m.Metrics.discarded = 1);
  check "example: 1 cancelled" (m.Metrics.cancelled = 1);
  check "example: positions are alice -10, bob 11, carol -1"
    (s.Replay.positions
    = [
        (acct "alice", -10);
        (acct "bob", 11);
        (acct "carol", -1);
      ]);
  check "example: final best ask is 105 (alice's remaining sell)"
    (match Order_book.best_ask s.Replay.book with
    | Some (Order.Limit { price; _ }) -> Price_ticks.to_int price = 105
    | _ -> false);
  check "example: final book has no bid" (Order_book.best_bid s.Replay.book = None)

(* --- 7. Duplicate resting order id rejected at validation ------------- *)

let () =
  (* Two limit orders share id 1; the first rests, the second reuses the id
     while the first is still on the book. *)
  let events, errors =
    Replay.parse "1,alice,1,sell,limit,101,10,\n2,bob,1,buy,limit,90,5,"
  in
  check "dup-resting: exactly one validation error" (List.length errors = 1);
  check "dup-resting: only the first event kept" (List.length events = 1);
  let e = List.hd errors in
  check "dup-resting: error names the duplicate id"
    (e.Replay.message = "duplicate order id #1");
  check "dup-resting: error points at the offending line 2"
    (e.Replay.line_number = 2);
  (* Running the surviving events must not raise and must reflect only the
     first, still-resting order. *)
  let s = Replay.run Replay.default_config events in
  check "dup-resting: book holds only the first order"
    (match Order_book.best_ask s.Replay.book with
    | Some (Order.Limit { id; quantity; _ }) ->
        Order_id.to_int id = 1 && Quantity.to_int quantity = 10
    | _ -> false)

(* --- 8. Duplicate incoming id after the first has fully filled -------- *)

let () =
  (* id 1 (alice's sell) fully fills against id 2 (bob's buy) and leaves the
     book; a later event reuses id 1 even though nothing with that id rests. *)
  let events, errors =
    Replay.parse
      "1,alice,1,sell,limit,100,5,\n\
       2,bob,2,buy,limit,100,5,\n\
       3,carol,1,buy,limit,100,5,"
  in
  check "dup-filled: exactly one validation error" (List.length errors = 1);
  check "dup-filled: the two distinct-id events survive" (List.length events = 2);
  let e = List.hd errors in
  check "dup-filled: error names the reused id"
    (e.Replay.message = "duplicate order id #1");
  check "dup-filled: error points at line 3" (e.Replay.line_number = 3);
  (* The survivors replay cleanly: id 1 fills id 2, book empties, positions net. *)
  let s = Replay.run Replay.default_config events in
  check "dup-filled: book empty after the pair trades"
    (Order_book.bids s.Replay.book = [] && Order_book.asks s.Replay.book = []);
  check "dup-filled: alice short 5" (Replay.position s (acct "alice") = -5);
  check "dup-filled: bob long 5" (Replay.position s (acct "bob") = 5)

(* --- 9. Duplicate ids produce a readable, well-formed error ----------- *)

(* Dependency-free substring test: is [needle] contained in [haystack]? *)
let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec at i =
    if i + nl > hl then false
    else if String.sub haystack i nl = needle then true
    else at (i + 1)
  in
  nl = 0 || at 0

let () =
  let _, errors = Replay.parse "1,alice,1,sell,limit,101,10,\n2,bob,1,buy,limit,90,5," in
  let e = List.hd errors in
  let rendered = Replay.string_of_parse_error e in
  (* Readable: mentions the reason, the line, and echoes the offending text. *)
  check "readable: rendering mentions 'duplicate'"
    (contains ~needle:"duplicate" rendered);
  check "readable: rendering mentions the line number"
    (contains ~needle:"line 2" rendered);
  check "readable: rendering echoes the offending line text"
    (contains ~needle:"2,bob,1" rendered)

(* --- 10. Rows out of timestamp order: time priority + determinism ----- *)

let () =
  (* alice's sell (timestamp 3) appears in the file before bob's sell
     (timestamp 1), both resting at price 100. Time priority puts bob ahead of
     alice despite arriving later in the file, so carol's buy of 4 must lift
     bob's order, not alice's. The stream is deliberately not timestamp-sorted;
     it is neither rejected nor reordered. *)
  let csv =
    "3,alice,1,sell,limit,100,5,\n\
     1,bob,2,sell,limit,100,5,\n\
     2,carol,3,buy,limit,100,4,"
  in
  let a = run_csv csv in
  let b = run_csv csv in
  check "out-of-order: no parse rejection for unsorted timestamps"
    (a.Replay.metrics.events_processed = 3);
  check "out-of-order: deterministic summary across runs"
    (Replay.summary_to_string a = Replay.summary_to_string b);
  check "out-of-order: deterministic positions across runs"
    (a.Replay.positions = b.Replay.positions);
  check "out-of-order: one fill of 4" (a.Replay.metrics.fills = 1 && a.Replay.metrics.traded_quantity = 4);
  check "out-of-order: earlier-timestamp bob (t1) filled, short 4"
    (Replay.position a (acct "bob") = -4);
  check "out-of-order: later-timestamp alice (t3) untouched, still flat"
    (Replay.position a (acct "alice") = 0);
  check "out-of-order: carol long 4" (Replay.position a (acct "carol") = 4);
  check "out-of-order: alice's full 5 still rests untouched"
    (match Order_book.find a.Replay.book (Order_id.of_int 1) with
    | Some (Order.Limit { quantity; _ }) -> Quantity.to_int quantity = 5
    | _ -> false);
  check "out-of-order: bob (earlier ts) is partially filled, 1 left as best ask"
    (match Order_book.best_ask a.Replay.book with
    | Some (Order.Limit { id; quantity; _ }) ->
        Order_id.to_int id = 2 && Quantity.to_int quantity = 1
    | _ -> false)

let () = Printf.printf "test_replay: %d checks passed\n" !checks
