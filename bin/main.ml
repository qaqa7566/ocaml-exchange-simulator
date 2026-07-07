(** Command-line entry point for the exchange simulator.

    Usage:
      {[ dune exec bin/main.exe -- replay <event-file> ]}

    Reads a recorded event stream (see [data/example_events.csv] for the format),
    replays it deterministically through the risk and matching engines, and
    prints a concise summary: events processed, accepted/rejected counts, fills
    and traded quantity, cancels, final best bid/ask, and final positions by
    account. *)

open Exchange

let usage = "usage: exchange replay <event-file>"

let run_replay path =
  match Replay.read_file path with
  | exception Sys_error msg ->
      prerr_endline ("error: " ^ msg);
      exit 1
  | contents ->
      let events, errors = Replay.parse contents in
      (match errors with
      | [] -> ()
      | _ ->
          List.iter
            (fun e -> prerr_endline ("error: " ^ Replay.string_of_parse_error e))
            errors;
          exit 1);
      let summary = Replay.run Replay.default_config events in
      Printf.printf "=== replay summary: %s ===\n%s\n" path
        (Replay.summary_to_string summary)

let () =
  match Array.to_list Sys.argv with
  | _ :: "replay" :: path :: [] -> run_replay path
  | _ ->
      prerr_endline usage;
      exit 2
