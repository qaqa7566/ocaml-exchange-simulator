# OCaml Exchange Simulator

An exchange simulator and risk engine written in OCaml. It models a
central limit order book (CLOB) with a matching engine, a pre-trade risk
engine, deterministic replay of recorded order streams, and benchmarking.

> **Status:** early scaffolding. The matching engine and supporting modules
> are stubbed out and not yet implemented.

## What it does

- **Central limit order book** with **price-time priority**: orders are
  matched best-price-first, and within a price level in **FIFO** (first-in,
  first-out) order of arrival.
- **Order types**: **market**, **limit**, and **cancel**.
- **Partial fills**: an incoming order may execute against several resting
  orders; any unfilled remainder of a limit order rests on the book, while a
  market order never rests.
- **Risk engine**: pre-trade **risk limits** (e.g. position, notional, and
  order-size caps) that can reject an order before it reaches the book.
- **Deterministic replay**: a recorded stream of orders always produces the
  exact same fills and final book state — no wall-clock time, no randomness.
- **Invariant-based tests**: tests assert structural and matching invariants
  rather than only hard-coded input/output pairs.
- **Benchmarking**: measures matching throughput and per-order processing
  cost.

## Folder structure

```
.
├── bin/     Command-line entry point (main.ml)
├── lib/     Core library (types, order, order book, matching engine,
│            risk engine, replay, metrics)
├── test/    Invariant-based tests for the order book and matching engine
├── bench/   Throughput and latency benchmarks
├── data/    Recorded order streams for replay (gitignored except .gitkeep)
├── dune-project
└── ocaml-exchange-simulator.opam
```

### Library modules (`lib/`)

- `types.ml` — shared domain types (side, order type, ids, price, quantity,
  timestamp).
- `order.ml` — the order representation and validation.
- `order_book.ml` — resting bids/asks organized by price-time priority.
- `matching_engine.ml` — matches incoming orders against the book, producing
  fills.
- `risk_engine.ml` — pre-trade risk-limit checks.
- `replay.ml` — deterministic replay of a recorded event stream.
- `metrics.ml` — run statistics for reporting and benchmarking.

## Core invariants

These hold at all times and are the basis for the test suite:

1. **Sorted book** — bids are ordered by descending price, asks by ascending
   price. The best bid and best ask are always at the front of their sides.
2. **Price-time priority** — among orders at the same price, the earliest to
   arrive is filled first (FIFO).
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

## Building

```sh
dune build          # build everything
dune test           # run the test suite
dune exec bin/main.exe   # run the simulator CLI
```
