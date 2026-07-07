(** Property-based tests for the matching engine (QCheck).

    Where {!test_matching_engine} pins down hand-picked scenarios, this suite
    drives {e generated} order streams through the engine and asserts that the
    matching contract holds for {e every} stream QCheck produces. Each stream is:

    - assigned unique order ids (the id is the 1-based arrival position);
    - built from valid, strictly positive quantities and prices;
    - a mix of buy/sell limit orders, buy/sell market orders, and cancels that
      target an earlier order id where one exists;
    - stamped with timestamps drawn independently of arrival order, so the
      stream routinely presents out-of-order and equal timestamps.

    Generators are deliberately small (short streams, tight price/quantity/
    timestamp ranges) so runs stay fast and QCheck's shrinker collapses any
    failure to a minimal counterexample. On failure the offending stream is
    printed one order per line via {!print_stream}. *)

open Exchange
open Types
module Book = Order_book
module ME = Matching_engine
open QCheck2

(* --- Construction helpers --------------------------------------------- *)

(* The generators only ever feed valid, strictly positive ints to the smart
   constructors, so [Error] is unreachable; surface it loudly if it ever fires. *)
let unwrap = function Ok v -> v | Error e -> failwith (Order.string_of_error e)

let mk_limit id side price qty ts =
  unwrap
    (Order.limit ~id:(Order_id.of_int id) ~side ~price ~quantity:qty
       ~timestamp:(Timestamp.of_int ts))

let mk_market id side qty ts =
  unwrap
    (Order.market ~id:(Order_id.of_int id) ~side ~quantity:qty
       ~timestamp:(Timestamp.of_int ts))

let mk_cancel id target ts =
  unwrap
    (Order.cancel ~id:(Order_id.of_int id) ~target:(Order_id.of_int target)
       ~timestamp:(Timestamp.of_int ts))

(* --- Generators ------------------------------------------------------- *)

let gen_side = Gen.oneof_list [ Buy; Sell ]

(* Tight ranges keep books shallow and collisions (same price, same timestamp)
   frequent, which is exactly where priority bugs hide. *)
let gen_price = Gen.int_range 1 20
let gen_qty = Gen.int_range 1 10

(* Timestamps are drawn independently of arrival position, so a later-arriving
   order routinely carries an earlier timestamp, and equal timestamps recur. *)
let gen_ts = Gen.int_range 0 40

(* One event at 1-based id [id]; [prior] is the number of orders already in the
   stream, so a cancel can target any id in [1..prior]. Limits dominate so the
   book actually fills up; cancels only appear once there is something to name. *)
let gen_event ~id ~prior =
  let open Gen in
  gen_ts >>= fun ts ->
  let limit_g =
    map
      (fun ((side, price), qty) -> mk_limit id side price qty ts)
      (pair (pair gen_side gen_price) gen_qty)
  in
  let market_g = map (fun (side, qty) -> mk_market id side qty ts) (pair gen_side gen_qty) in
  let weighted =
    [ (5, limit_g); (2, market_g) ]
    @
    if prior >= 1 then [ (3, map (fun target -> mk_cancel id target ts) (int_range 1 prior)) ]
    else []
  in
  oneof_weighted weighted

(* A whole stream: [n] events with ids 1..n assigned by arrival position, which
   makes ids unique by construction. *)
let gen_stream =
  let open Gen in
  int_range 0 30 >>= fun n ->
  let rec build i acc =
    if i >= n then return (List.rev acc)
    else gen_event ~id:(i + 1) ~prior:i >>= fun o -> build (i + 1) (o :: acc)
  in
  build 0 []

(* --- Readable failure diagnostics ------------------------------------- *)

let string_of_order (o : Order.t) =
  match o with
  | Order.Limit { id; side; price; quantity; timestamp } ->
      Printf.sprintf "#%d limit  %-4s %2d @ %-3d t%d" (Order_id.to_int id)
        (string_of_side side) (Quantity.to_int quantity) (Price_ticks.to_int price)
        (Timestamp.to_int timestamp)
  | Order.Market { id; side; quantity; timestamp } ->
      Printf.sprintf "#%d market %-4s %2d       t%d" (Order_id.to_int id) (string_of_side side)
        (Quantity.to_int quantity) (Timestamp.to_int timestamp)
  | Order.Cancel { id; target; timestamp } ->
      Printf.sprintf "#%d cancel -> #%d           t%d" (Order_id.to_int id)
        (Order_id.to_int target) (Timestamp.to_int timestamp)

let print_stream orders =
  match orders with
  | [] -> "<empty stream>"
  | _ -> "\n  " ^ String.concat "\n  " (List.map string_of_order orders) ^ "\n"

(* --- Engine driver + inspection --------------------------------------- *)

(* Fold the whole stream through the engine, returning every step's
   (order, result) in arrival order. Purely functional: each step threads the
   book returned by the previous one. *)
let steps orders =
  let rec go book acc = function
    | [] -> List.rev acc
    | o :: rest ->
        let r = ME.process book o in
        go r.ME.book ((o, r) :: acc) rest
  in
  go Book.empty [] orders

let resting b = Book.bids b @ Book.asks b
let resting_ids b = List.map (fun o -> Order_id.to_int (Order.id o)) (resting b)

let price_of = function
  | Order.Limit { price; _ } -> Price_ticks.to_int price
  | _ -> failwith "resting order is not a limit"

let fill_qty f = Quantity.to_int f.ME.quantity

let uncrossed b =
  match (Book.best_bid b, Book.best_ask b) with
  | Some bid, Some ask -> price_of bid < price_of ask
  | _ -> true

let rec nondecreasing = function a :: (b :: _ as rest) -> a <= b && nondecreasing rest | _ -> true
let no_duplicates l = List.length (List.sort_uniq compare l) = List.length l

(* Within each price level the book claims earliest-timestamp-first, so the
   timestamps down a level must be nondecreasing (exact ties break by arrival
   order, which still leaves timestamps nondecreasing). *)
let levels_time_ordered b =
  let snap = Book.snapshot b in
  let level_ok (_, orders) =
    nondecreasing (List.map (fun o -> Timestamp.to_int (Order.timestamp o)) orders)
  in
  List.for_all level_ok snap.Book.bid_levels && List.for_all level_ok snap.Book.ask_levels

(* A deterministic signature of a whole run: every outcome and fill in order,
   then the final book. Two runs of the same stream must produce equal strings. *)
let run_signature orders =
  let buf = Buffer.create 256 in
  let book =
    List.fold_left
      (fun book o ->
        let r = ME.process book o in
        Buffer.add_string buf (ME.string_of_outcome r.ME.outcome);
        List.iter
          (fun f ->
            Buffer.add_string buf
              (Printf.sprintf " [%d<-%d @%d x%d]"
                 (Order_id.to_int f.ME.resting_id)
                 (Order_id.to_int f.ME.incoming_id)
                 (Price_ticks.to_int f.ME.price) (fill_qty f)))
          r.ME.fills;
        Buffer.add_char buf '\n';
        r.ME.book)
      Book.empty orders
  in
  Buffer.add_string buf (Book.to_string book);
  Buffer.contents buf

let market_ids orders =
  List.filter_map
    (function Order.Market { id; _ } -> Some (Order_id.to_int id) | _ -> None)
    orders

let submitted_qty orders =
  List.fold_left
    (fun a o ->
      match o with
      | Order.Limit { quantity; _ } | Order.Market { quantity; _ } -> a + Quantity.to_int quantity
      | Order.Cancel _ -> a)
    0 orders

let executed_qty stps =
  List.fold_left
    (fun a (_, r) -> a + List.fold_left (fun a f -> a + fill_qty f) 0 r.ME.fills)
    0 stps

(* --- Properties ------------------------------------------------------- *)

let count = 1000

let t_uncrossed =
  Test.make ~count ~name:"book is uncrossed after every processed event" ~print:print_stream
    gen_stream (fun orders -> List.for_all (fun (_, r) -> uncrossed r.ME.book) (steps orders))

let t_market_never_rests =
  Test.make ~count ~name:"market orders never rest" ~print:print_stream gen_stream (fun orders ->
      let outcome_ok (o, r) =
        match o with
        | Order.Market _ -> ( match r.ME.outcome with ME.Rested _ -> false | _ -> true)
        | _ -> true
      in
      let markets = market_ids orders in
      let never_resting (_, r) =
        not (List.exists (fun id -> List.mem id markets) (resting_ids r.ME.book))
      in
      let stps = steps orders in
      List.for_all outcome_ok stps && List.for_all never_resting stps)

let t_quantity_conserved =
  Test.make ~count ~name:"executed quantity never exceeds submitted quantity" ~print:print_stream
    gen_stream (fun orders -> executed_qty (steps orders) <= submitted_qty orders)

let t_deterministic =
  Test.make ~count ~name:"replaying the same stream twice yields identical output/state"
    ~print:print_stream gen_stream (fun orders ->
      String.equal (run_signature orders) (run_signature orders))

let t_time_priority =
  Test.make ~count ~name:"same-price timestamp priority is respected in the book"
    ~print:print_stream gen_stream (fun orders ->
      List.for_all (fun (_, r) -> levels_time_ordered r.ME.book) (steps orders))

let t_no_duplicate_ids =
  Test.make ~count ~name:"no duplicate resting ids survive in the book" ~print:print_stream
    gen_stream (fun orders ->
      List.for_all (fun (_, r) -> no_duplicates (resting_ids r.ME.book)) (steps orders))

let () =
  QCheck_base_runner.run_tests_main
    [
      t_uncrossed;
      t_market_never_rests;
      t_quantity_conserved;
      t_deterministic;
      t_time_priority;
      t_no_duplicate_ids;
    ]
