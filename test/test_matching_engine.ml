(** Invariant-based tests for the matching engine (Phase 3).

    Dependency-free in the same style as {!test_order_book}: each check is an
    [assert]-style guard with a label, so a failure aborts the run and names the
    offending case. These exercise the matching contract — price-time priority,
    partial fills and remainders, better-price execution, market orders never
    resting, cancellation, quantity conservation, and the "no crossed book"
    invariant that only holds once matching exists. *)

open Exchange
open Types
module Book = Order_book
module ME = Matching_engine

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("FAILED: " ^ name)

(* --- Construction helpers --------------------------------------------- *)

let oid = Order_id.of_int
let ts = Timestamp.of_int
let ok = function Ok v -> v | Error _ -> failwith "expected Ok"
let qty n = Option.get (Quantity.create n)

(* [id] doubles as the timestamp so arrival order is unambiguous for FIFO. *)
let mk_limit ~id ~side ~price ~qty =
  ok (Order.limit ~id:(oid id) ~side ~price ~quantity:qty ~timestamp:(ts id))

(* A limit whose timestamp is set independently of its id, so time priority and
   the order of construction/insertion can be made to disagree. *)
let mk_limit_ts ~id ~time ~side ~price ~qty =
  ok (Order.limit ~id:(oid id) ~side ~price ~quantity:qty ~timestamp:(ts time))

let mk_market ~id ~side ~qty =
  ok (Order.market ~id:(oid id) ~side ~quantity:qty ~timestamp:(ts id))

let mk_cancel ~id ~target =
  ok (Order.cancel ~id:(oid id) ~target:(oid target) ~timestamp:(ts id))

(* A resting-only book, built by adding limits directly (no matching). *)
let rest orders = List.fold_left (fun b o -> ok (Book.add b o)) Book.empty orders

(* --- Fill / book inspection ------------------------------------------- *)

let fill_resting f = Order_id.to_int f.ME.resting_id
let fill_incoming f = Order_id.to_int f.ME.incoming_id
let fill_price f = Price_ticks.to_int f.ME.price
let fill_qty f = Quantity.to_int f.ME.quantity
let fills_summary fills = List.map (fun f -> (fill_resting f, fill_price f, fill_qty f)) fills

let price_of = function
  | Order.Limit { price; _ } -> Price_ticks.to_int price
  | _ -> failwith "not a resting limit"

let qty_of = function
  | Order.Limit { quantity; _ } -> Quantity.to_int quantity
  | _ -> failwith "not a resting limit"

let ask_ids b = List.map (fun o -> Order_id.to_int (Order.id o)) (Book.asks b)
let best_ask_id b = Option.map (fun o -> Order_id.to_int (Order.id o)) (Book.best_ask b)
let best_bid_id b = Option.map (fun o -> Order_id.to_int (Order.id o)) (Book.best_bid b)

(* Total resting units across both sides — for conservation checks. *)
let total_qty b =
  List.fold_left (fun a o -> a + qty_of o) 0 (Book.bids b)
  + List.fold_left (fun a o -> a + qty_of o) 0 (Book.asks b)

(* The core structural invariant matching must preserve. *)
let uncrossed b =
  match (Book.best_bid b, Book.best_ask b) with
  | Some bid, Some ask -> price_of bid < price_of ask
  | _ -> true

(* --- 1. Full limit fill ----------------------------------------------- *)

let () =
  let b = rest [ mk_limit ~id:1 ~side:Sell ~price:100 ~qty:10 ] in
  let r = ME.process b (mk_limit ~id:2 ~side:Buy ~price:100 ~qty:10) in
  check "full-fill: outcome Filled" (r.ME.outcome = ME.Filled);
  check "full-fill: one fill" (List.length r.ME.fills = 1);
  check "full-fill: fill details" (fills_summary r.ME.fills = [ (1, 100, 10) ]);
  check "full-fill: incoming id recorded" (fill_incoming (List.hd r.ME.fills) = 2);
  check "full-fill: book empty" (Book.asks r.ME.book = [] && Book.bids r.ME.book = []);
  check "full-fill: uncrossed" (uncrossed r.ME.book)

(* --- 2. Partial fill with remainder resting --------------------------- *)

let () =
  let b = rest [ mk_limit ~id:1 ~side:Sell ~price:100 ~qty:4 ] in
  let r = ME.process b (mk_limit ~id:2 ~side:Buy ~price:100 ~qty:10) in
  check "partial-rest: outcome Rested 6" (r.ME.outcome = ME.Rested (qty 6));
  check "partial-rest: one fill of 4" (fills_summary r.ME.fills = [ (1, 100, 4) ]);
  check "partial-rest: ask fully consumed" (Book.asks r.ME.book = []);
  check "partial-rest: remainder rests as bid" (best_bid_id r.ME.book = Some 2);
  check "partial-rest: remainder qty is 6" (qty_of (Option.get (Book.best_bid r.ME.book)) = 6);
  check "partial-rest: uncrossed" (uncrossed r.ME.book)

(* --- 3. Partial fill across multiple price levels --------------------- *)

let () =
  let b =
    rest
      [
        mk_limit ~id:1 ~side:Sell ~price:100 ~qty:3;
        mk_limit ~id:2 ~side:Sell ~price:101 ~qty:3;
        mk_limit ~id:3 ~side:Sell ~price:102 ~qty:3;
      ]
  in
  let r = ME.process b (mk_limit ~id:10 ~side:Buy ~price:101 ~qty:5) in
  check "multi-level: outcome Filled" (r.ME.outcome = ME.Filled);
  check "multi-level: fills walk price levels"
    (fills_summary r.ME.fills = [ (1, 100, 3); (2, 101, 2) ]);
  check "multi-level: level 100 gone, 101 partly left, 102 untouched" (ask_ids r.ME.book = [ 2; 3 ]);
  check "multi-level: id2 has 1 left" (qty_of (Option.get (Book.best_ask r.ME.book)) = 1);
  check "multi-level: id3 (over limit) untouched" (qty_of (List.nth (Book.asks r.ME.book) 1) = 3);
  check "multi-level: no remainder rested (buy exhausted)" (Book.bids r.ME.book = []);
  check "multi-level: uncrossed" (uncrossed r.ME.book)

(* --- 4. Better-price execution for a limit order ---------------------- *)

let () =
  let b = rest [ mk_limit ~id:1 ~side:Sell ~price:103 ~qty:5 ] in
  let r = ME.process b (mk_limit ~id:2 ~side:Buy ~price:105 ~qty:5) in
  check "better-price: outcome Filled" (r.ME.outcome = ME.Filled);
  check "better-price: executed at resting 103, not limit 105"
    (fills_summary r.ME.fills = [ (1, 103, 5) ]);
  check "better-price: uncrossed" (uncrossed r.ME.book)

(* --- 5. Non-crossing limit order rests untouched ---------------------- *)

let () =
  let b = rest [ mk_limit ~id:1 ~side:Sell ~price:105 ~qty:5 ] in
  let r = ME.process b (mk_limit ~id:2 ~side:Buy ~price:100 ~qty:5) in
  check "no-cross: no fills" (r.ME.fills = []);
  check "no-cross: whole order rests" (r.ME.outcome = ME.Rested (qty 5));
  check "no-cross: ask untouched" (ask_ids r.ME.book = [ 1 ]);
  check "no-cross: bid rested at 100" (best_bid_id r.ME.book = Some 2);
  check "no-cross: uncrossed (100 < 105)" (uncrossed r.ME.book)

(* --- 6. Market order never rests -------------------------------------- *)

let () =
  (* More market demand than liquidity: partial fill, remainder discarded. *)
  let b = rest [ mk_limit ~id:1 ~side:Sell ~price:100 ~qty:3 ] in
  let r = ME.process b (mk_market ~id:2 ~side:Buy ~qty:10) in
  check "market-discard: outcome Discarded 7" (r.ME.outcome = ME.Discarded (qty 7));
  check "market-discard: filled what liquidity existed" (fills_summary r.ME.fills = [ (1, 100, 3) ]);
  check "market-discard: nothing rested" (Book.bids r.ME.book = [] && Book.asks r.ME.book = []);
  check "market-discard: uncrossed" (uncrossed r.ME.book);
  (* Enough liquidity: market fully fills, resting remainder stays on book. *)
  let b = rest [ mk_limit ~id:1 ~side:Sell ~price:100 ~qty:10 ] in
  let r = ME.process b (mk_market ~id:2 ~side:Buy ~qty:4) in
  check "market-fill: outcome Filled" (r.ME.outcome = ME.Filled);
  check "market-fill: fill of 4" (fills_summary r.ME.fills = [ (1, 100, 4) ]);
  check "market-fill: 6 left resting on the ask" (qty_of (Option.get (Book.best_ask r.ME.book)) = 6)

(* --- 7. FIFO at the same price ---------------------------------------- *)

let () =
  let b =
    rest
      [
        mk_limit ~id:1 ~side:Sell ~price:100 ~qty:2;
        mk_limit ~id:2 ~side:Sell ~price:100 ~qty:2;
        mk_limit ~id:3 ~side:Sell ~price:100 ~qty:2;
      ]
  in
  let r = ME.process b (mk_limit ~id:10 ~side:Buy ~price:100 ~qty:3) in
  check "fifo: earliest arrivals fill first"
    (fills_summary r.ME.fills = [ (1, 100, 2); (2, 100, 1) ]);
  check "fifo: partially-filled head keeps priority" (ask_ids r.ME.book = [ 2; 3 ]);
  check "fifo: id2 reduced to 1, still ahead of id3"
    (qty_of (Option.get (Book.best_ask r.ME.book)) = 1);
  check "fifo: uncrossed" (uncrossed r.ME.book)

(* --- 7b. Time priority: earlier timestamp fills first ----------------- *)

let () =
  (* Three sells at the same price, rested in id order 1,2,3 but with timestamps
     30,10,20. Time priority is 2 (t10), 3 (t20), 1 (t30). A buy of 3 must fill
     the two earliest timestamps, not the two earliest-added. *)
  let b =
    rest
      [
        mk_limit_ts ~id:1 ~time:30 ~side:Sell ~price:100 ~qty:2;
        mk_limit_ts ~id:2 ~time:10 ~side:Sell ~price:100 ~qty:2;
        mk_limit_ts ~id:3 ~time:20 ~side:Sell ~price:100 ~qty:2;
      ]
  in
  let r = ME.process b (mk_limit ~id:10 ~side:Buy ~price:100 ~qty:3) in
  check "time-priority: fills earliest timestamps first (id2 then id3)"
    (fills_summary r.ME.fills = [ (2, 100, 2); (3, 100, 1) ]);
  check "time-priority: id1 (latest timestamp) untouched, id3 partially left"
    (ask_ids r.ME.book = [ 3; 1 ]);
  check "time-priority: best ask is the partially-filled id3 (t20)"
    (best_ask_id r.ME.book = Some 3);
  check "time-priority: uncrossed" (uncrossed r.ME.book)

(* --- 7c. Partial fill preserves the original timestamp priority -------- *)

let () =
  (* id2 (t10) and id1 (t30) rest at 100. A first buy of 3 fully consumes id2
     and takes 1 from id1, leaving id1 partially filled. A second buy must still
     see id1 in its original time-priority slot (t30), and id5 (t40) rested
     between the two buys must sit behind it — proving the partial fill did not
     reset the resting order's timestamp/priority to "now". *)
  let b =
    rest
      [
        mk_limit_ts ~id:1 ~time:30 ~side:Sell ~price:100 ~qty:4;
        mk_limit_ts ~id:2 ~time:10 ~side:Sell ~price:100 ~qty:2;
      ]
  in
  let r = ME.process b (mk_limit ~id:3 ~side:Buy ~price:100 ~qty:3) in
  check "partial-priority: earliest timestamp id2 filled first, then id1"
    (fills_summary r.ME.fills = [ (2, 100, 2); (1, 100, 1) ]);
  check "partial-priority: id1 left with 3 resting" (qty_of (Option.get (Book.best_ask r.ME.book)) = 3);
  (* A newly rested sell with a later timestamp than id1 (t40 > t30). *)
  let b2 = ok (Book.add r.ME.book (mk_limit_ts ~id:5 ~time:40 ~side:Sell ~price:100 ~qty:2)) in
  check "partial-priority: partially-filled id1 keeps its slot ahead of newer id5"
    (ask_ids b2 = [ 1; 5 ]);
  let r2 = ME.process b2 (mk_limit ~id:6 ~side:Buy ~price:100 ~qty:3) in
  check "partial-priority: next buy hits id1 (t30) before id5 (t40)"
    (fills_summary r2.ME.fills = [ (1, 100, 3) ]);
  check "partial-priority: only id5 remains after id1 exhausted"
    (ask_ids r2.ME.book = [ 5 ])

(* --- 8. Sell-side symmetry -------------------------------------------- *)

let () =
  (* Incoming sell hits the highest bid; execution at the resting bid price. *)
  let b = rest [ mk_limit ~id:1 ~side:Buy ~price:100 ~qty:5 ] in
  let r = ME.process b (mk_limit ~id:2 ~side:Sell ~price:100 ~qty:5) in
  check "sell: outcome Filled" (r.ME.outcome = ME.Filled);
  check "sell: fill against the bid" (fills_summary r.ME.fills = [ (1, 100, 5) ]);
  check "sell: incoming side recorded" ((List.hd r.ME.fills).ME.incoming_side = Sell);
  check "sell: book empty" (Book.bids r.ME.book = [] && Book.asks r.ME.book = []);
  (* Partial sell: remainder rests as an ask, execution improves on the limit. *)
  let b = rest [ mk_limit ~id:1 ~side:Buy ~price:100 ~qty:3 ] in
  let r = ME.process b (mk_limit ~id:2 ~side:Sell ~price:98 ~qty:5) in
  check "sell-partial: outcome Rested 2" (r.ME.outcome = ME.Rested (qty 2));
  check "sell-partial: bid consumed" (Book.bids r.ME.book = []);
  check "sell-partial: remainder rests as ask" (best_ask_id r.ME.book = Some 2);
  check "sell-partial: better-price fill at bid 100, not limit 98"
    (fill_price (List.hd r.ME.fills) = 100);
  check "sell-partial: remainder rests at limit 98"
    (price_of (Option.get (Book.best_ask r.ME.book)) = 98);
  check "sell-partial: uncrossed" (uncrossed r.ME.book)

(* --- 9. Cancel via the matching engine -------------------------------- *)

let () =
  let b = rest [ mk_limit ~id:1 ~side:Buy ~price:100 ~qty:5 ] in
  let r = ME.process b (mk_cancel ~id:2 ~target:1) in
  check "cancel: outcome Cancelled target" (r.ME.outcome = ME.Cancelled (oid 1));
  check "cancel: no fills" (r.ME.fills = []);
  check "cancel: target removed" (Book.find r.ME.book (oid 1) = None && Book.bids r.ME.book = []);
  (* Unknown target: a clear miss, book untouched. *)
  let r2 = ME.process r.ME.book (mk_cancel ~id:3 ~target:999) in
  check "cancel: unknown target reported" (r2.ME.outcome = ME.Cancel_miss (oid 999));
  check "cancel: unknown target leaves no fills" (r2.ME.fills = []);
  check "cancel: unknown target leaves book unchanged" (Book.bids r2.ME.book = [])

(* --- 10. Quantity conservation across fills --------------------------- *)

let () =
  let b =
    rest
      [
        mk_limit ~id:1 ~side:Sell ~price:100 ~qty:3;
        mk_limit ~id:2 ~side:Sell ~price:101 ~qty:3;
      ]
  in
  let before = total_qty b in
  let incoming_qty = 5 in
  let r = ME.process b (mk_limit ~id:10 ~side:Buy ~price:101 ~qty:incoming_qty) in
  let filled = List.fold_left (fun a f -> a + fill_qty f) 0 r.ME.fills in
  let after = total_qty r.ME.book in
  (* Units leaving the resting book equal the units filled. *)
  check "conservation: resting delta = fills" (before - after = filled);
  (* Every incoming unit is either filled or (for a limit) resting; none vanish. *)
  let resting_remainder = match r.ME.outcome with ME.Rested q -> Quantity.to_int q | _ -> 0 in
  check "conservation: incoming accounted for" (filled + resting_remainder = incoming_qty);
  check "conservation: exact split" (filled = 5 && resting_remainder = 0)

(* --- 11. Book is uncrossed after matching ----------------------------- *)

let () =
  (* Start from an uncrossed two-sided book, then send a crossing limit buy that
     lifts one ask level and rests the remainder; the book must stay uncrossed. *)
  let b =
    rest
      [
        mk_limit ~id:1 ~side:Buy ~price:99 ~qty:5;
        mk_limit ~id:2 ~side:Sell ~price:101 ~qty:4;
        mk_limit ~id:3 ~side:Sell ~price:102 ~qty:4;
      ]
  in
  check "uncrossed-before: setup is uncrossed" (uncrossed b);
  let r = ME.process b (mk_limit ~id:10 ~side:Buy ~price:101 ~qty:6) in
  check "uncrossed-after: crossing buy lifts level 101"
    (fills_summary r.ME.fills = [ (2, 101, 4) ]);
  check "uncrossed-after: remainder rests as bid at 101"
    (best_bid_id r.ME.book = Some 10 && price_of (Option.get (Book.best_bid r.ME.book)) = 101);
  check "uncrossed-after: best ask now 102" (best_ask_id r.ME.book = Some 3);
  check "uncrossed-after: book not crossed (101 < 102)" (uncrossed r.ME.book)

let () = Printf.printf "test_matching_engine: %d checks passed\n" !checks
