(** Invariant-based tests for the limit order book (resting only; no matching).

    Dependency-free: each check is an [assert]-style guard with a label, so a
    failure aborts the run and names the offending case. These exercise the
    structural invariants of the resting book — price ordering, FIFO within a
    level, cancellation, and level cleanup — plus the guard that only limit
    orders may rest.

    Note: there is deliberately no "no crossed book" assertion here. This module
    performs no matching, so a bid added at or above the best ask legitimately
    rests and crosses the book; that invariant only holds once
    {!Exchange.Matching_engine} has matched. *)

open Exchange
open Types
module Book = Order_book

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("FAILED: " ^ name)

(* --- Construction helpers --------------------------------------------- *)

let oid = Order_id.of_int
let ts = Timestamp.of_int

let ok = function Ok v -> v | Error _ -> failwith "expected Ok"

(* A validated limit order. [t] is both the id and the timestamp so that
   arrival order is unambiguous in priority checks. *)
let mk_limit ~id ~side ~price ~qty =
  ok (Order.limit ~id:(oid id) ~side ~price ~quantity:qty ~timestamp:(ts id))

(* A limit order whose timestamp is set independently of its id, so that arrival
   order (the [add] order) and time priority (the timestamp) can differ. *)
let mk_limit_ts ~id ~time ~side ~price ~qty =
  ok (Order.limit ~id:(oid id) ~side ~price ~quantity:qty ~timestamp:(ts time))

let add_ok book order = ok (Book.add book order)

(* The ids resting at a given side, in full priority order (best price first,
   FIFO within a level). *)
let bid_ids book = List.map (fun o -> Order_id.to_int (Order.id o)) (Book.bids book)
let ask_ids book = List.map (fun o -> Order_id.to_int (Order.id o)) (Book.asks book)

(* The distinct price levels of a side, best price first. *)
let bid_prices book = List.map fst (Book.snapshot book).bid_levels
let ask_prices book = List.map fst (Book.snapshot book).ask_levels

let best_bid_id book = Option.map (fun o -> Order_id.to_int (Order.id o)) (Book.best_bid book)
let best_ask_id book = Option.map (fun o -> Order_id.to_int (Order.id o)) (Book.best_ask book)

(* --- Empty book ------------------------------------------------------- *)

let () =
  let b = Book.empty in
  check "empty: no best bid" (Book.best_bid b = None);
  check "empty: no best ask" (Book.best_ask b = None);
  check "empty: no bids" (Book.bids b = []);
  check "empty: no asks" (Book.asks b = []);
  check "empty: no bid levels" (bid_prices b = []);
  check "empty: no ask levels" (ask_prices b = [])

(* --- Price ordering --------------------------------------------------- *)

let () =
  (* Insert bids out of order; they must read back highest-first. *)
  let b = Book.empty in
  let b = add_ok b (mk_limit ~id:1 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit ~id:2 ~side:Buy ~price:102 ~qty:5) in
  let b = add_ok b (mk_limit ~id:3 ~side:Buy ~price:101 ~qty:5) in
  check "bids: sorted highest-first" (bid_prices b = [ 102; 101; 100 ]);
  check "bids: best bid is highest price" (best_bid_id b = Some 2)

let () =
  (* Insert asks out of order; they must read back lowest-first. *)
  let b = Book.empty in
  let b = add_ok b (mk_limit ~id:1 ~side:Sell ~price:105 ~qty:5) in
  let b = add_ok b (mk_limit ~id:2 ~side:Sell ~price:103 ~qty:5) in
  let b = add_ok b (mk_limit ~id:3 ~side:Sell ~price:104 ~qty:5) in
  check "asks: sorted lowest-first" (ask_prices b = [ 103; 104; 105 ]);
  check "asks: best ask is lowest price" (best_ask_id b = Some 2)

(* --- FIFO within a price level ---------------------------------------- *)

let () =
  (* Three bids at the same price; earlier arrival (smaller id/ts) rests ahead. *)
  let b = Book.empty in
  let b = add_ok b (mk_limit ~id:10 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit ~id:11 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit ~id:12 ~side:Buy ~price:100 ~qty:5) in
  check "fifo: single price level" (bid_prices b = [ 100 ]);
  check "fifo: arrival order preserved" (bid_ids b = [ 10; 11; 12 ]);
  check "fifo: best bid is earliest at price" (best_bid_id b = Some 10)

(* --- Time priority: earlier timestamp rests ahead, even if added later - *)

let () =
  (* Three sells at the same price added in id order 1,2,3 but with timestamps
     30,10,20. Time priority must order them by timestamp: 2 (t10), 3 (t20),
     1 (t30) — i.e. NOT the order they were added. *)
  let b = Book.empty in
  let b = add_ok b (mk_limit_ts ~id:1 ~time:30 ~side:Sell ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit_ts ~id:2 ~time:10 ~side:Sell ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit_ts ~id:3 ~time:20 ~side:Sell ~price:100 ~qty:5) in
  check "time-priority: single price level" (ask_prices b = [ 100 ]);
  check "time-priority: ordered by timestamp, not insertion order"
    (ask_ids b = [ 2; 3; 1 ]);
  check "time-priority: best ask is earliest timestamp (id 2, t10)"
    (best_ask_id b = Some 2)

(* --- Equal timestamps fall back to arrival order ---------------------- *)

let () =
  (* Two bids share timestamp 5; a third has an earlier timestamp 1. The t1
     order jumps ahead; the two t5 orders keep the order they were added in. *)
  let b = Book.empty in
  let b = add_ok b (mk_limit_ts ~id:1 ~time:5 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit_ts ~id:2 ~time:5 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit_ts ~id:3 ~time:1 ~side:Buy ~price:100 ~qty:5) in
  check "tie: earlier timestamp first, then arrival order for the tie"
    (bid_ids b = [ 3; 1; 2 ]);
  check "tie: best bid is the earliest-timestamp order (id 3, t1)"
    (best_bid_id b = Some 3);
  (* All three equal timestamps: pure arrival order, deterministically. *)
  let b = Book.empty in
  let b = add_ok b (mk_limit_ts ~id:10 ~time:7 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit_ts ~id:11 ~time:7 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit_ts ~id:12 ~time:7 ~side:Buy ~price:100 ~qty:5) in
  check "tie: equal timestamps preserve arrival order" (bid_ids b = [ 10; 11; 12 ])

(* --- Cancellation removes only the target ----------------------------- *)

let () =
  let b = Book.empty in
  let b = add_ok b (mk_limit ~id:10 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit ~id:11 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit ~id:12 ~side:Buy ~price:100 ~qty:5) in
  let b = ok (Book.cancel b (oid 11)) in
  check "cancel: only target removed" (bid_ids b = [ 10; 12 ]);
  check "cancel: target no longer found" (Book.find b (oid 11) = None);
  check "cancel: others still found" (Book.find b (oid 10) <> None && Book.find b (oid 12) <> None);
  check "cancel: unknown id rejected"
    (Book.cancel b (oid 999) = Error (Book.Unknown_order_id (oid 999)))

(* --- Empty price level disappears after final cancellation ------------ *)

let () =
  let b = Book.empty in
  let b = add_ok b (mk_limit ~id:1 ~side:Buy ~price:100 ~qty:5) in
  let b = add_ok b (mk_limit ~id:2 ~side:Sell ~price:200 ~qty:5) in
  check "cleanup: level present before cancel" (bid_prices b = [ 100 ]);
  let b = ok (Book.cancel b (oid 1)) in
  check "cleanup: bid level gone after final cancel" (bid_prices b = []);
  check "cleanup: no best bid after final cancel" (Book.best_bid b = None);
  check "cleanup: other side untouched" (ask_prices b = [ 200 ])

(* --- Market orders never enter the book ------------------------------- *)

let () =
  let b = Book.empty in
  let mkt = ok (Order.market ~id:(oid 1) ~side:Buy ~quantity:5 ~timestamp:(ts 1)) in
  check "market: rejected by add" (Book.add b mkt = Error Book.Not_a_limit_order);
  let b = match Book.add b mkt with Ok b -> b | Error _ -> b in
  check "market: book still empty" (Book.bids b = [] && Book.asks b = [])

(* --- Cancel requests never enter the book ----------------------------- *)

let () =
  let b = Book.empty in
  let cxl = ok (Order.cancel ~id:(oid 1) ~target:(oid 2) ~timestamp:(ts 1)) in
  check "cancel-order: rejected by add" (Book.add b cxl = Error Book.Not_a_limit_order);
  let b = match Book.add b cxl with Ok b -> b | Error _ -> b in
  check "cancel-order: book still empty" (Book.bids b = [] && Book.asks b = []);
  check "cancel-order: not findable" (Book.find b (oid 1) = None)

(* --- Duplicate id is rejected ----------------------------------------- *)

let () =
  let b = Book.empty in
  let b = add_ok b (mk_limit ~id:1 ~side:Buy ~price:100 ~qty:5) in
  check "duplicate: same id rejected"
    (Book.add b (mk_limit ~id:1 ~side:Buy ~price:101 ~qty:5)
    = Error (Book.Duplicate_order_id (oid 1)));
  check "duplicate: book unchanged" (bid_prices b = [ 100 ])

let () = Printf.printf "test_order_book: %d checks passed\n" !checks
