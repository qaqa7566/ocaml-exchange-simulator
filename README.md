# OCaml Exchange Simulator

An exchange simulator and risk engine written in OCaml. It models a
central limit order book (CLOB) with a matching engine, a pre-trade risk
engine, and deterministic replay of recorded order streams.

> **Status:** the order book, matching engine, risk engine, and a deterministic
> end-to-end replay with a CLI are implemented. Async and networking are not
> built yet.

## What it does

- **Central limit order book** with **price-time priority**: orders are
  matched best-price-first, and within a price level by **earliest timestamp
  first**; exact timestamp ties break by deterministic arrival order.
- **Order types**: **market**, **limit**, and **cancel**.
- **Partial fills**: an incoming order may execute against several resting
  orders; any unfilled remainder of a limit order rests on the book, while a
  market order never rests.
- **Risk engine**: pre-trade **risk limits** (e.g. position, notional, and
  order-size caps) that can reject an order before it reaches the book.
- **Deterministic replay**: a recorded stream of orders always produces the
  exact same fills and final book state — no wall-clock time, no randomness.
- **Property-based tests**: alongside the hand-written scenario suites,
  [QCheck](https://github.com/c-cube/qcheck) generates thousands of random
  order streams (unique ids, valid positive prices/quantities, buy/sell limit
  and market orders, cancels targeting earlier ids, out-of-order timestamps)
  and asserts the matching invariants hold for every one — shrinking any
  failure to a minimal, printed counterexample.

## Folder structure

```
.
├── bin/     Command-line entry point (main.ml)
├── lib/     Core library (types, order, order book, matching engine,
│            risk engine, replay, metrics)
├── test/    Scenario and property-based (QCheck) tests for the engine
├── data/    Recorded order streams for replay (only the example is committed)
├── dune-project
└── ocaml-exchange-simulator.opam
```

### Library modules (`lib/`)

- `types.ml` — shared domain types: `side` plus typed scalar wrappers
  (`Order_id`, `Price_ticks`, `Quantity`, `Timestamp`) whose smart
  constructors keep quantities and prices strictly positive.
- `order.ml` — the order representation (`Limit` / `Market` / `Cancel`, each
  carrying only the fields that kind needs) and its validating constructors.
- `order_book.ml` — resting bids/asks organized by price-time priority.
- `matching_engine.ml` — matches incoming orders against the book, producing
  fills.
- `risk_engine.ml` — pre-trade risk-limit checks.
- `replay.ml` — deterministic replay of a recorded event stream.
- `metrics.ml` — run statistics for reporting.

## Core invariants

These hold at all times and are the basis for the test suite — the QCheck
property tests in `test/test_properties.ml` assert several of them (no crossed
book, quantity conservation, market orders never resting, timestamp priority,
unique resting ids, replay determinism) against randomly generated streams:

1. **Sorted book** — bids are ordered by descending price, asks by ascending
   price. The best bid and best ask are always at the front of their sides.
2. **Price-time priority** — orders are ranked by price first, then by
   timestamp: among orders at the same price, the one with the earliest
   timestamp is filled first. Orders may arrive out of timestamp order; each
   still takes its correct time-priority slot. Exact timestamp ties break by
   deterministic arrival order.
3. **No crossed book** — after matching completes, the best bid price is
   strictly less than the best ask price; any crossing quantity has already
   traded.
4. **Quantity conservation** — for every match, total quantity filled equals
   the sum of quantities removed from the two sides; nothing is created or
   destroyed.
5. **Market orders never rest** — a market order either fills (fully or
   partially) or is cancelled for lack of liquidity; it never joins the book.
6. **Limit price respected** — a limit order never trades at a price worse than
   its limit; any unfilled remainder rests at its limit price.
7. **Determinism** — replaying the same input stream yields identical fills and
   final book state.
8. **Risk limits enforced** — an order that would breach a configured risk
   limit is rejected before it can affect the book.

## Building and running

```sh
dune build                                        # build everything
dune test                                         # run the test suite
dune exec bin/main.exe -- replay data/example_events.csv   # replay an event stream
```

`replay` reads a recorded event stream, drives it deterministically through the
risk and matching engines, and prints a summary — events processed,
accepted/rejected, fills and traded quantity, cancels, the final best bid/ask,
and final positions by account. Replaying the same file always prints the same
summary (no wall-clock time, no randomness). Running the shipped example prints:

```
=== replay summary: data/example_events.csv ===
events processed : 9
accepted         : 8
rejected         : 1
fills            : 3
traded quantity  : 15
filled orders    : 2
rested orders    : 4
discarded orders : 1
cancelled        : 1
cancel misses    : 0
best bid         : none
best ask         : 105
positions:
  alice: -10
  bob: 11
  carol: -1
```

## Event-file format

An event stream is a small CSV, one event per line. Lines beginning with `#`
and blank lines are ignored; `#` comment headers are the recommended way to
label columns (there is no separate header row). Order ids must be unique
across the run. Columns:

```
timestamp,account,order_id,side,type,price,quantity,target
```

| column      | limit            | market            | cancel            |
| ----------- | ---------------- | ----------------- | ----------------- |
| `timestamp` | logical int (drives time priority) | ✓ | ✓ |
| `account`   | owner id (string)| ✓                 | ✓                 |
| `order_id`  | int, unique      | ✓                 | ✓                 |
| `side`      | `buy` / `sell`   | `buy` / `sell`    | *(blank)*         |
| `type`      | `limit`          | `market`          | `cancel`          |
| `price`     | int ticks        | *(blank)*         | *(blank)*         |
| `quantity`  | int              | int               | *(blank)*         |
| `target`    | *(blank)*        | *(blank)*         | `order_id` to cancel |

A cell that does not apply is left blank (or `-`). A **limit** order rests any
unfilled remainder on the book; a **market** order never rests (any unfillable
remainder is discarded); a **cancel** removes its `target` from the book.

Example (see [`data/example_events.csv`](data/example_events.csv)):

```
# ts,account,order_id,side,type,price,quantity,target
1,alice,1,sell,limit,101,10,
2,bob,2,buy,limit,100,5,
3,carol,3,buy,limit,101,4,
4,carol,4,sell,market,,8,
5,bob,5,buy,limit,101,6,
6,alice,6,sell,limit,105,10,
7,bob,7,buy,limit,103,4,
8,bob,8,,cancel,,,7
9,carol,9,buy,limit,100,5000,
```

### Design notes

- **Account ownership of resting orders.** A fill names order ids, but an
  `Order.t` carries no account, so applying a fill to *both* counterparties'
  positions needs an `order_id → account` mapping. Rather than add an account
  field to every order and the book (touching every module), the replay layer —
  the single point every order passes through — keeps that map itself. Positions
  therefore move only from real fills, attributed to real owners.
- **Market-order reference price.** The risk engine needs a reference price to
  value a market order (which has no price of its own). Replay values it at the
  top of the opposing book it will trade against (best ask for a buy, best bid
  for a sell), falling back to a configured default when that side is empty. A
  limit order is valued at its own price.
