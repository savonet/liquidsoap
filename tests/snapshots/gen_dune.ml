(* Emit one pair of rules per case: run snapshot.exe over it, then diff the
   result against the checked-in expected file. See README.md. *)

let is_case f =
  match Filename.extension f with
    | ".liq" -> true
    (* Cases that are deliberately unparsable are named `.invalid-liq`, so that
       the repo-wide `**/*.liq` glob the external tree-sitter and lezer grammars
       are checked against does not pick them up. Files meant to be pulled in by
       a case's `%include` use `.liq-inc`, for the same reason. *)
    | ".invalid-liq" -> true
    | _ -> false

(* [flag] is passed to snapshot.exe ahead of the case; [prefix] keeps the
   generated target names of the two suites apart. *)
let emit_rules ~cases_dir ~expected_dir ~prefix ~flag case =
  let target = prefix ^ Filename.remove_extension case in
  Printf.printf
    {|
(rule
 (target %s.actual)
 (deps
  (:case %s/%s)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to %%{target} (run %%{snapshot} %s%%{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff %s/%s.expected %s.actual)))
|}
    target cases_dir case flag expected_dir
    (Filename.remove_extension case)
    target

let () =
  let location = Sys.getcwd () in
  let suite ~cases_dir ~expected_dir ~prefix ~flag =
    List.iter
      (emit_rules ~cases_dir ~expected_dir ~prefix ~flag)
      (List.filter is_case (Build_tools.read_files ~location cases_dir))
  in
  suite ~cases_dir:"cases" ~expected_dir:"expected" ~prefix:"" ~flag:"";
  suite ~cases_dir:"cases_canonical" ~expected_dir:"expected_canonical"
    ~prefix:"canonical_" ~flag:"--canonical "
