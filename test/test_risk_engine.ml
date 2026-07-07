(** Tests for the pre-trade risk engine.

    Dependency-free, in the same style as the other suites: each check is an
    [assert]-like predicate labelled so a failure aborts the run and names the
    offending case. Covers acceptance within limits, each rejection cause,
    boundary cases exactly on a limit, market-order notional handling, and the
    purity of {!Exchange.Risk_engine.check}. *)

open Exchange
open Types
open Risk_engine

let checks = ref 0

let check_that name cond =
  incr checks;
  if not cond then failwith ("FAILED: " ^ name)

let acct = Account_id.of_string "alice"
let ts = Timestamp.of_int 0
let oid = Order_id.of_int

let ok = function Ok v -> v | Error _ -> failwith "expected Ok order"
let ok_q n = match Quantity.create n with Some q -> q | None -> assert false

let mk_limit ~side ~price ~quantity =
  ok (Order.limit ~id:(oid 1) ~side ~price ~quantity ~timestamp:ts)

let mk_market ~side ~quantity =
  ok (Order.market ~id:(oid 1) ~side ~quantity ~timestamp:ts)

let is_accepted = function Accepted -> true | Rejected _ -> false

(* Generous limits so a single dimension can be isolated in each test. *)
let base_limits =
  Limits.create ~max_order_quantity:100 ~max_position:1000
    ~max_notional:1_000_000

(* --- Acceptance within limits ---------------------------------------- *)

let () =
  let st = empty base_limits in
  let o = mk_limit ~side:Buy ~price:10 ~quantity:5 in
  check_that "accepted: order within all limits"
    (is_accepted (check st ~account:acct o))

(* --- Max order quantity ---------------------------------------------- *)

let () =
  let st =
    empty
      (Limits.create ~max_order_quantity:10 ~max_position:1000
         ~max_notional:1_000_000)
  in
  (* Exactly at the cap is accepted; one over is rejected. *)
  check_that "boundary: order quantity exactly at max accepted"
    (is_accepted
       (check st ~account:acct (mk_limit ~side:Buy ~price:1 ~quantity:10)));
  check_that "reject: order quantity over max"
    (check st ~account:acct (mk_limit ~side:Buy ~price:1 ~quantity:11)
    = Rejected (Order_quantity_exceeded { limit = 10; requested = 11 }))

(* --- Position limits: long and short breaches ------------------------ *)

let () =
  let lim =
    Limits.create ~max_order_quantity:100 ~max_position:50
      ~max_notional:1_000_000
  in
  (* Start long 45; a buy of 5 lands exactly at +50 (accepted), a buy of 6
     projects +51 and breaches on the long side. *)
  let st = apply_fill (empty lim) ~account:acct ~side:Buy (ok_q 45) in
  check_that "boundary: long position exactly at max accepted"
    (is_accepted
       (check st ~account:acct (mk_limit ~side:Buy ~price:1 ~quantity:5)));
  check_that "reject: long position breach"
    (check st ~account:acct (mk_limit ~side:Buy ~price:1 ~quantity:6)
    = Rejected (Position_limit_exceeded { limit = 50; projected = 51 }));
  (* Start short 45; a sell of 5 lands exactly at -50 (accepted), a sell of 6
     projects -51 and breaches on the short side. *)
  let st = apply_fill (empty lim) ~account:acct ~side:Sell (ok_q 45) in
  check_that "boundary: short position exactly at max accepted"
    (is_accepted
       (check st ~account:acct (mk_limit ~side:Sell ~price:1 ~quantity:5)));
  check_that "reject: short position breach"
    (check st ~account:acct (mk_limit ~side:Sell ~price:1 ~quantity:6)
    = Rejected (Position_limit_exceeded { limit = 50; projected = -51 }))

(* --- Notional limits ------------------------------------------------- *)

let () =
  (* max_notional 100; position cap high enough not to interfere; price 10. *)
  let lim =
    Limits.create ~max_order_quantity:100 ~max_position:1000 ~max_notional:100
  in
  let st = empty lim in
  (* projected 10 units * price 10 = 100, exactly at the cap: accepted. *)
  check_that "boundary: notional exactly at max accepted"
    (is_accepted
       (check st ~account:acct (mk_limit ~side:Buy ~price:10 ~quantity:10)));
  (* projected 11 units * price 10 = 110 > 100: rejected. *)
  check_that "reject: notional breach"
    (check st ~account:acct (mk_limit ~side:Buy ~price:10 ~quantity:11)
    = Rejected (Notional_limit_exceeded { limit = 100; projected = 11 }))

(* --- Kill switch ----------------------------------------------------- *)

let () =
  let st = set_kill_switch (empty base_limits) acct true in
  check_that "reject: kill switch engaged"
    (check st ~account:acct (mk_limit ~side:Buy ~price:10 ~quantity:1)
    = Rejected (Kill_switch_engaged acct));
  (* A cancel is risk-reducing and admitted even under the kill switch. *)
  let cancel = ok (Order.cancel ~id:(oid 2) ~target:(oid 1) ~timestamp:ts) in
  check_that "accepted: cancel admitted under kill switch"
    (is_accepted (check st ~account:acct cancel));
  (* A different account is unaffected. *)
  let bob = Account_id.of_string "bob" in
  check_that "accepted: other account unaffected by kill switch"
    (is_accepted
       (check st ~account:bob (mk_limit ~side:Buy ~price:10 ~quantity:1)))

(* --- Market-order notional handling ---------------------------------- *)

let () =
  let lim =
    Limits.create ~max_order_quantity:100 ~max_position:1000 ~max_notional:100
  in
  let st = empty lim in
  let reference_price =
    match Price_ticks.create 10 with Some p -> p | None -> assert false
  in
  (* Market order valued at the supplied reference price: 10 units * 10 = 100
     exactly at cap, accepted. *)
  check_that "accepted: market order at reference notional cap"
    (is_accepted
       (check st ~account:acct ~reference_price (mk_market ~side:Buy ~quantity:10)));
  (* 11 units * 10 = 110 > 100: rejected. *)
  check_that "reject: market order breaches reference notional"
    (check st ~account:acct ~reference_price (mk_market ~side:Buy ~quantity:11)
    = Rejected (Notional_limit_exceeded { limit = 100; projected = 11 }));
  (* No reference price: a market order cannot be valued and is rejected. *)
  check_that "reject: market order without reference price"
    (check st ~account:acct (mk_market ~side:Buy ~quantity:1)
    = Rejected No_reference_price)

(* --- Purity: state is unchanged after checks ------------------------- *)

let () =
  let st = apply_fill (empty base_limits) ~account:acct ~side:Buy (ok_q 20) in
  let before = position st acct in
  (* Run a mix of accepted and rejected checks. *)
  let _ = check st ~account:acct (mk_limit ~side:Buy ~price:10 ~quantity:5) in
  let _ = check st ~account:acct (mk_limit ~side:Buy ~price:10 ~quantity:1000) in
  let _ = check st ~account:acct (mk_market ~side:Sell ~quantity:5) in
  check_that "purity: position unchanged after checks" (position st acct = before);
  check_that "purity: kill switch unchanged after checks"
    (not (is_kill_switch st acct))

(* --- Fill-driven position updates ------------------------------------ *)

let () =
  let st = empty base_limits in
  let st = apply_fill st ~account:acct ~side:Buy (ok_q 30) in
  check_that "apply_fill: buy increases position" (position st acct = 30);
  let st = apply_fill st ~account:acct ~side:Sell (ok_q 50) in
  check_that "apply_fill: sell decreases position through zero into short"
    (position st acct = -20)

let () = Printf.printf "test_risk_engine: %d checks passed\n" !checks
