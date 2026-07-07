(** Deterministic replay.

    Reads a recorded stream of events and drives them through the pre-trade
    {!Risk_engine} and the {!Matching_engine} in a reproducible way: a given
    input always yields identical fills, positions, book state, and {!Metrics}.
    There is no wall-clock time and no randomness — logical timestamps come from
    the event stream and events are processed strictly in file order.

    {2 Account ownership of resting orders}

    Applying a fill to risk needs the {e account} on both sides, but a
    {!Matching_engine.fill} only names the order ids (resting and incoming) and
    the aggressor's side — and an {!Order.t} carries no account. Rather than
    thread an account through every order constructor and the whole book (which
    would touch every existing module and test), the replay — the single entry
    point through which every order passes — keeps an [Order_id -> Account_id]
    {!owner map}. Order ids are unique across a run, so this map resolves both
    counterparties of any fill. This is genuine risk accounting: positions move
    only from real fills, attributed to the real owners.

    A model extension (an account field on the order, stored on the book) would
    only be needed if orders could enter the book through a path the replay does
    not see, or if ids were reused within a run. Neither holds here.

    {2 Reference price for market orders}

    A market order has no price, so {!Risk_engine.check} needs a caller-supplied
    reference price. The strategy here values the order at the top of the
    opposing book it is about to trade against (best ask for a buy, best bid for
    a sell) — the most-likely execution price — falling back to a configured
    [default_reference_price] when that side is empty. A limit order is valued at
    its own price (no override passed), matching the risk engine's default. *)

open Types
module ME = Matching_engine

module Owner_map = Map.Make (Order_id)
module Order_id_set = Set.Make (Order_id)
module Account_set = Set.Make (Risk_engine.Account_id)

(** Replay configuration: the risk limits every account is checked against, plus
    the fallback reference price used to value a market order when the opposing
    side of the book is empty. *)
type config = {
  limits : Risk_engine.Limits.t;
  default_reference_price : Price_ticks.t;
}

let price_ticks n =
  match Price_ticks.create n with
  | Some p -> p
  | None -> invalid_arg "Replay: default_reference_price must be positive"

(** A generous default suitable for the checked-in example: caps high enough
    that ordinary orders pass, with one obviously oversized order rejected. *)
let default_config =
  {
    limits =
      Risk_engine.Limits.create ~max_order_quantity:1000 ~max_position:1000
        ~max_notional:10_000_000;
    default_reference_price = price_ticks 100;
  }

(** A single parsed event: the order and the account that submitted it. *)
type event = { account : Risk_engine.Account_id.t; order : Order.t }

(** A line that could not be parsed, with enough context to report it. *)
type parse_error = { line_number : int; text : string; message : string }

let string_of_parse_error e =
  Printf.sprintf "line %d: %s  [%s]" e.line_number e.message e.text

(* --- Parsing ---------------------------------------------------------- *)

(* A blank cell (empty or "-") means "field not applicable". *)
let is_blank s = s = "" || s = "-"

let split_fields line =
  List.map String.trim (String.split_on_char ',' line) |> Array.of_list

let field cols i = if i < Array.length cols then cols.(i) else ""

let parse_int ~ctx s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None -> Error (Printf.sprintf "%s: %S is not an integer" ctx s)

let parse_side s =
  match String.lowercase_ascii s with
  | "buy" | "b" -> Ok Buy
  | "sell" | "s" -> Ok Sell
  | _ -> Error (Printf.sprintf "side must be buy or sell, got %S" s)

(* Turn an [Order] construction result into a parse-level message. *)
let of_order_result = function
  | Ok o -> Ok o
  | Error e -> Error (Order.string_of_error e)

let parse_order cols =
  let ( let* ) = Result.bind in
  let* ts_i = parse_int ~ctx:"timestamp" (field cols 0) in
  let account_s = field cols 1 in
  let* () =
    if is_blank account_s then Error "account is required" else Ok ()
  in
  let* id_i = parse_int ~ctx:"order_id" (field cols 2) in
  let side_s = field cols 3 in
  let type_s = String.lowercase_ascii (field cols 4) in
  let price_s = field cols 5 in
  let qty_s = field cols 6 in
  let target_s = field cols 7 in
  let id = Order_id.of_int id_i in
  let timestamp = Timestamp.of_int ts_i in
  let account = Risk_engine.Account_id.of_string account_s in
  let order =
    match type_s with
    | "limit" ->
        let* side = parse_side side_s in
        let* price = parse_int ~ctx:"price" price_s in
        let* quantity = parse_int ~ctx:"quantity" qty_s in
        of_order_result (Order.limit ~id ~side ~price ~quantity ~timestamp)
    | "market" ->
        let* side = parse_side side_s in
        let* quantity = parse_int ~ctx:"quantity" qty_s in
        of_order_result (Order.market ~id ~side ~quantity ~timestamp)
    | "cancel" ->
        let* target_i = parse_int ~ctx:"target" target_s in
        of_order_result
          (Order.cancel ~id ~target:(Order_id.of_int target_i) ~timestamp)
    | other -> Error (Printf.sprintf "unknown order type %S" other)
  in
  let* order = order in
  Ok { account; order }

(** [parse_line line_number text] parses one raw line. A comment ([#...]) or
    blank line is [Ok None]; a valid event is [Ok (Some event)]; anything else
    is a typed {!parse_error}. *)
let parse_line line_number text : (event option, parse_error) result =
  let trimmed = String.trim text in
  if trimmed = "" || String.length trimmed > 0 && trimmed.[0] = '#' then Ok None
  else
    match parse_order (split_fields trimmed) with
    | Ok event -> Ok (Some event)
    | Error message -> Error { line_number; text = trimmed; message }

(** [parse contents] splits a whole file into lines and parses each, returning
    the events in order alongside any parse errors (also in order).

    Order ids must be unique across the whole stream: reusing a submitted id —
    even after the first order has fully filled and left the book — is reported
    as a validation error and the offending event is dropped, so it never
    reaches matching (where a duplicate resting id would otherwise trip an
    internal assertion). The first occurrence of an id is kept; later reuses are
    rejected. *)
let parse contents : event list * parse_error list =
  let lines = String.split_on_char '\n' contents in
  let events, errors, _, _ =
    List.fold_left
      (fun (events, errors, seen, n) line ->
        match parse_line n line with
        | Ok None -> (events, errors, seen, n + 1)
        | Ok (Some e) ->
            let id = Order.id e.order in
            if Order_id_set.mem id seen then
              let err =
                {
                  line_number = n;
                  text = String.trim line;
                  message =
                    Printf.sprintf "duplicate order id %s" (Order_id.pp id);
                }
              in
              (events, err :: errors, seen, n + 1)
            else (e :: events, errors, Order_id_set.add id seen, n + 1)
        | Error err -> (events, err :: errors, seen, n + 1))
      ([], [], Order_id_set.empty, 1) lines
  in
  (List.rev events, List.rev errors)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* --- Driving ---------------------------------------------------------- *)

type driver_state = {
  book : Order_book.t;
  risk : Risk_engine.t;
  owners : Risk_engine.Account_id.t Owner_map.t;
  accounts : Account_set.t;
  metrics : Metrics.t;
}

let opposite = function Buy -> Sell | Sell -> Buy

(* Price of a resting order (always a limit; the book stores nothing else). *)
let resting_price = function
  | Order.Limit { price; _ } -> Some price
  | Order.Market _ | Order.Cancel _ -> None

(* The market-order reference price strategy: value at the top of the opposing
   book, falling back to the configured default when that side is empty. *)
let reference_price_for config book side =
  let opposing_top =
    match side with
    | Buy -> Order_book.best_ask book
    | Sell -> Order_book.best_bid book
  in
  match Option.bind opposing_top resting_price with
  | Some p -> p
  | None -> config.default_reference_price

(* Apply one fill to both counterparties' positions, resolving each side's
   account through the owner map. Owners are always known here (both orders
   passed through the replay), but a missing owner is skipped rather than
   misattributed — genuine accounting over faked. *)
let apply_fill_both owners risk (f : ME.fill) =
  let move risk side id =
    match Owner_map.find_opt id owners with
    | Some account -> Risk_engine.apply_fill risk ~account ~side f.ME.quantity
    | None -> risk
  in
  let risk = move risk f.ME.incoming_side f.ME.incoming_id in
  move risk (opposite f.ME.incoming_side) f.ME.resting_id

let step config state { account; order } =
  let state =
    {
      state with
      accounts = Account_set.add account state.accounts;
      metrics = Metrics.count_event state.metrics;
    }
  in
  let decision =
    match order with
    | Order.Market { side; _ } ->
        let reference_price = reference_price_for config state.book side in
        Risk_engine.check state.risk ~account ~reference_price order
    | Order.Limit _ | Order.Cancel _ ->
        Risk_engine.check state.risk ~account order
  in
  match decision with
  | Risk_engine.Rejected _ ->
      (* A rejected order never reaches the book: no owner recorded, book,
         positions, and owner map all unchanged. *)
      { state with metrics = Metrics.record_rejected state.metrics }
  | Risk_engine.Accepted ->
      (* Record ownership before matching so the incoming order resolves in its
         own fills. Cancels carry no tradable position, so they need no owner. *)
      let owners =
        match order with
        | Order.Limit _ | Order.Market _ ->
            Owner_map.add (Order.id order) account state.owners
        | Order.Cancel _ -> state.owners
      in
      let result = ME.process state.book order in
      let risk =
        List.fold_left (apply_fill_both owners) state.risk result.ME.fills
      in
      {
        state with
        book = result.ME.book;
        risk;
        owners;
        metrics = Metrics.record_accepted state.metrics result;
      }

(** The outcome of a replay run. [positions] lists every account seen (including
    cancel-only accounts, which stay flat) with its final net position, sorted
    ascending by account id for deterministic output. *)
type summary = {
  metrics : Metrics.t;
  book : Order_book.t;
  positions : (Risk_engine.Account_id.t * int) list;
}

(** [run config events] drives [events] through the engines in order and returns
    the final metrics, book, and per-account positions. *)
let run config events : summary =
  let init =
    {
      book = Order_book.empty;
      risk = Risk_engine.empty config.limits;
      owners = Owner_map.empty;
      accounts = Account_set.empty;
      metrics = Metrics.empty;
    }
  in
  let final = List.fold_left (step config) init events in
  let positions =
    Account_set.elements final.accounts
    |> List.map (fun a -> (a, Risk_engine.position final.risk a))
  in
  { metrics = final.metrics; book = final.book; positions }

(** [position summary account] is the final net position recorded for [account]
    in [summary] (0 if the account never appeared). *)
let position summary account =
  List.find_opt
    (fun (a, _) -> Risk_engine.Account_id.equal a account)
    summary.positions
  |> Option.map snd
  |> Option.value ~default:0

(* --- Rendering -------------------------------------------------------- *)

let best_price side book =
  let order =
    match side with Buy -> Order_book.best_bid book | Sell -> Order_book.best_ask book
  in
  match Option.bind order resting_price with
  | Some p -> string_of_int (Price_ticks.to_int p)
  | None -> "none"

(** A stable, human-readable rendering of a run summary: metrics, top of book,
    and final positions by account. Identical for identical runs. *)
let summary_to_string summary =
  let positions_lines =
    if summary.positions = [] then [ "  (none)" ]
    else
      List.map
        (fun (a, p) ->
          Printf.sprintf "  %s: %d" (Risk_engine.Account_id.to_string a) p)
        summary.positions
  in
  String.concat "\n"
    (Metrics.to_string summary.metrics
     :: Printf.sprintf "best bid         : %s" (best_price Buy summary.book)
     :: Printf.sprintf "best ask         : %s" (best_price Sell summary.book)
     :: "positions:" :: positions_lines)
