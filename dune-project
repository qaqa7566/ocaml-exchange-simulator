(lang dune 3.0)

(name ocaml-exchange-simulator)

(generate_opam_files true)

(package
 (name ocaml-exchange-simulator)
 (synopsis "An exchange simulator and risk engine in OCaml")
 (description
  "A deterministic limit order book, matching engine, and risk engine with replay and benchmarking support.")
 (depends
  (ocaml
   (>= 4.14))
  dune
  (qcheck-core
   (and
    (>= 0.20)
    :with-test))))
