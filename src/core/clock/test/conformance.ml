let groups =
  [
    ("core", Core_checks.run);
    ("unify", Unify_checks.run);
    ("failure", Failure_checks.run);
    ("sync", Sync_checks.run);
    ("pacing", Pacing_checks.run);
    ("two_workers", Pool_checks.two_workers);
    ("one_worker", Pool_checks.one_worker);
    ("shutdown", Shutdown_checks.orderly);
    ("stuck", Shutdown_checks.stuck);
    ("report", Report_checks.run);
  ]

let () =
  match Array.to_list Sys.argv with
    | [_; group] when List.mem_assoc group groups ->
        (List.assoc group groups) ();
        Harness.finish group
    | _ ->
        Printf.eprintf "usage: conformance (%s)\n"
          (String.concat "|" (List.map fst groups));
        exit 2
