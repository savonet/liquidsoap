(* Runs the conformance suite, spec/tests.md §10.

   [runner --profile <p> --expect-pass <n> [--allow-skip <id>]... <test>...]
   runs every requirement of every test program in a process of its own,
   with a time limit, and fails unless exactly [n] requirements passed and
   every other one is an allowed skip.

   Requirements named [control.*] are planted defects. The ones of the
   profile are run first and must fail: a sanitised run counts only once the
   sanitiser was seen to report. *)

let time_limit = 120.

let controls = function
  | "asan" -> ["control.asan-overflow"; "control.lsan-leak"]
  | "gcstress" -> ["control.gc-unrooted"]
  | _ -> []

type outcome = Passed | Skipped | Failed of string

(* The exit code test/harness.ml gives a skipped requirement. *)
let skipped = 77

let describe_status = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "killed by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

(* The output and the status of [program argument]; [None] past the time
   limit. *)
let execute ~env program argument =
  let output_read, output_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process_env program [| program; argument |] env Unix.stdin
      output_write output_write
  in
  Unix.close output_write;
  let deadline = Unix.gettimeofday () +. time_limit in
  let output = Buffer.create 1024 and chunk = Bytes.create 4096 in
  let rec read () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0. then false
    else (
      match Unix.select [output_read] [] [] remaining with
        | [], _, _ -> false
        | _ -> (
            match Unix.read output_read chunk 0 (Bytes.length chunk) with
              | 0 -> true
              | length ->
                  Buffer.add_subbytes output chunk 0 length;
                  read ()))
  in
  let finished = read () in
  if not finished then Unix.kill pid Sys.sigkill;
  let _, status = Unix.waitpid [] pid in
  Unix.close output_read;
  (Buffer.contents output, if finished then Some status else None)

let run_requirement ~env program id =
  let output, status = execute ~env program id in
  let outcome =
    match status with
      | Some (Unix.WEXITED 0) -> Passed
      | Some (Unix.WEXITED code) when code = skipped -> Skipped
      | Some status -> Failed (describe_status status)
      | None ->
          Failed (Printf.sprintf "no result after %.0f seconds" time_limit)
  in
  (outcome, output)

let list_requirements ~env program =
  match execute ~env program "--list" with
    | output, Some (Unix.WEXITED 0) ->
        String.split_on_char '\n' output |> List.filter (fun id -> id <> "")
    | _ -> failwith (program ^ " --list failed")

(* The runtime shuts down at exit: values still alive release their native
   memory, and what a leak detector reports is a leak. *)
let environment () =
  Array.append
    [| "OCAMLRUNPARAM=c,v=0"; "ASAN_OPTIONS=detect_leaks=1" |]
    (Unix.environment ())

let is_control id = String.starts_with ~prefix:"control." id

let () =
  let profile = ref "dev" and expected_passes = ref (-1) in
  let allowed_skips = ref [] and programs = ref [] in
  Arg.parse
    [
      ("--profile", Arg.Set_string profile, "build profile");
      ("--expect-pass", Arg.Set_int expected_passes, "number of passes required");
      ( "--allow-skip",
        Arg.String (fun id -> allowed_skips := id :: !allowed_skips),
        "requirement that may be skipped" );
    ]
    (fun program ->
      let program =
        if Filename.is_implicit program then
          Filename.concat Filename.current_dir_name program
        else program
      in
      programs := program :: !programs)
    "runner [options] <test program>...";
  let env = environment () in
  let requirements =
    List.concat_map
      (fun program ->
        List.map (fun id -> (program, id)) (list_requirements ~env program))
      (List.rev !programs)
  in
  let failures = ref [] and passes = ref 0 and skips = ref 0 in
  let fail id reason output =
    Printf.printf "FAILED   %s: %s\n%s\n%!" id reason output;
    failures := id :: !failures
  in
  List.iter
    (fun id ->
      match
        List.find_opt (fun (_, candidate) -> candidate = id) requirements
      with
        | None -> fail id "the planted defect does not exist" ""
        | Some (program, _) -> (
            match run_requirement ~env program id with
              | Failed reason, _ ->
                  Printf.printf "DETECTED %s (%s)\n%!" id reason
              | _, output ->
                  fail id "the planted defect was not detected" output))
    (controls !profile);
  List.iter
    (fun (program, id) ->
      if not (is_control id) then (
        match run_requirement ~env program id with
          | Passed, _ ->
              incr passes;
              Printf.printf "passed   %s\n%!" id
          | Skipped, output when List.mem id !allowed_skips ->
              incr skips;
              Printf.printf "SKIPPED  %s: %s%!" id output
          | Skipped, output ->
              fail id "skipped, and no skip is allowed here" output
          | Failed reason, output -> fail id reason output))
    requirements;
  Printf.printf "%d passed, %d skipped, %d failed\n" !passes !skips
    (List.length !failures);
  if !passes <> !expected_passes then (
    Printf.printf "%d passes were expected: the suite did not run as intended\n"
      !expected_passes;
    exit 1);
  if !failures <> [] then exit 1
