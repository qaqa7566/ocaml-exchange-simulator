(** Construction and validation tests for the core domain types.

    Dependency-free: each check is an [assert] with a label printed on success,
    so a failure aborts the run and points at the offending case. Covers the
    typed scalar wrappers ({!Exchange.Types}) and the order smart constructors
    ({!Exchange.Order}). *)

open Exchange
open Types

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("FAILED: " ^ name)

let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true

(* --- Typed scalar wrappers -------------------------------------------- *)

let () =
  (* Quantity is positive-only by construction. *)
  check "quantity: positive accepted"
    (match Quantity.create 5 with Some q -> Quantity.to_int q = 5 | None -> false);
  check "quantity: zero rejected" (Quantity.create 0 = None);
  check "quantity: negative rejected" (Quantity.create (-3) = None);

  (* Price_ticks is positive-only by construction. *)
  check "price: positive accepted"
    (match Price_ticks.create 100 with
    | Some p -> Price_ticks.to_int p = 100
    | None -> false);
  check "price: zero rejected" (Price_ticks.create 0 = None);
  check "price: negative rejected" (Price_ticks.create (-1) = None);

  (* Order_id / Timestamp round-trip and comparison. *)
  check "order_id: round-trip" (Order_id.to_int (Order_id.of_int 7) = 7);
  check "order_id: equal"
    (Order_id.equal (Order_id.of_int 7) (Order_id.of_int 7));
  check "order_id: distinct"
    (not (Order_id.equal (Order_id.of_int 7) (Order_id.of_int 8)));
  check "timestamp: ordering"
    (Timestamp.compare (Timestamp.of_int 1) (Timestamp.of_int 2) < 0)

(* --- Order smart constructors ----------------------------------------- *)

let ts = Timestamp.of_int 0
let oid = Order_id.of_int
let side = Buy

let () =
  (* Limit orders. *)
  check "limit: valid"
    (is_ok (Order.limit ~id:(oid 1) ~side ~price:100 ~quantity:10 ~timestamp:ts));
  check "limit: nonpositive quantity rejected"
    (Order.limit ~id:(oid 1) ~side ~price:100 ~quantity:0 ~timestamp:ts
    = Error (Order.Nonpositive_quantity 0));
  check "limit: negative quantity rejected"
    (Order.limit ~id:(oid 1) ~side ~price:100 ~quantity:(-2) ~timestamp:ts
    = Error (Order.Nonpositive_quantity (-2)));
  check "limit: nonpositive price rejected"
    (Order.limit ~id:(oid 1) ~side ~price:0 ~quantity:10 ~timestamp:ts
    = Error (Order.Nonpositive_price 0));
  check "limit: negative price rejected"
    (Order.limit ~id:(oid 1) ~side ~price:(-5) ~quantity:10 ~timestamp:ts
    = Error (Order.Nonpositive_price (-5)));
  (* Quantity is validated before price. *)
  check "limit: quantity checked before price"
    (Order.limit ~id:(oid 1) ~side ~price:(-5) ~quantity:0 ~timestamp:ts
    = Error (Order.Nonpositive_quantity 0));

  (* Market orders (no price). *)
  check "market: valid"
    (is_ok (Order.market ~id:(oid 2) ~side ~quantity:10 ~timestamp:ts));
  check "market: nonpositive quantity rejected"
    (Order.market ~id:(oid 2) ~side ~quantity:0 ~timestamp:ts
    = Error (Order.Nonpositive_quantity 0));

  (* Cancel orders. *)
  check "cancel: valid"
    (is_ok (Order.cancel ~id:(oid 3) ~target:(oid 1) ~timestamp:ts));
  check "cancel: self-cancel rejected"
    (is_error (Order.cancel ~id:(oid 3) ~target:(oid 3) ~timestamp:ts))

(* --- Accessors -------------------------------------------------------- *)

let () =
  let ok = function Ok v -> v | Error _ -> failwith "expected Ok" in
  let l = ok (Order.limit ~id:(oid 1) ~side ~price:100 ~quantity:10 ~timestamp:ts) in
  let c = ok (Order.cancel ~id:(oid 3) ~target:(oid 1) ~timestamp:ts) in
  check "id: reads submitting order id" (Order_id.equal (Order.id l) (oid 1));
  check "id: cancel id is its own, not target"
    (Order_id.equal (Order.id c) (oid 3))

let () = Printf.printf "test_order: %d checks passed\n" !checks
