let timeout = 10 * 60
let test = Sys.argv.(1)
let cmd = Sys.argv.(2)
let args = try Array.sub Sys.argv 2 (Array.length Sys.argv - 2) with _ -> [||]
let stdin_file = Filename.null
let log_files = ref []

let cleanup () =
  List.iter (fun logfile -> try Unix.unlink logfile with _ -> ()) !log_files

let warning_prefix, error_prefix =
  if Sys.getenv_opt "GITHUB_ACTIONS" <> None then
    ( Printf.sprintf "::warning file=%s,title=Test skipped::" test,
      Printf.sprintf "::error file=%s,title=Test failed::" test )
  else ("", "")

let () = Console.color_conf := `Always
let colorized_test = Console.colorize [`white; `bold] test
let colorized_timeout = Console.colorize [`magenta; `bold] "[timeout]"
let colorized_ok = Console.colorize [`green; `bold] "[ok]"
let colorized_skipped = Console.colorize [`yellow; `bold] "[skipped]"
let colorized_failed = Console.colorize [`red; `bold] "[failed]"

let run_process ~action cmd args =
  let start_time = Unix.time () in
  let logfile = Filename.temp_file "test" test in
  log_files := logfile :: !log_files;
  let stdin = Unix.openfile stdin_file [Unix.O_RDWR] 0o644 in
  let stdout = Unix.openfile logfile [Unix.O_RDWR; Unix.O_TRUNC] 0o644 in

  let print_log () =
    try
      Unix.close stdout;
      let buffer_size = 1024 in
      let buffer = Bytes.create buffer_size in
      let fd = Unix.openfile logfile [Unix.O_RDONLY] 0 in
      let rec loop () =
        match Unix.read fd buffer 0 buffer_size with
          | 0 -> ()
          | _ ->
              Printf.eprintf "%s" (Bytes.unsafe_to_string buffer);
              loop ()
      in
      loop ();
      print_newline ();
      Unix.close fd
    with _ -> ()
  in

  let runtime () =
    let runtime = Unix.time () -. start_time in
    let min = int_of_float (runtime /. 60.) in
    let sec = runtime -. (float min *. 60.) in
    (min, int_of_float sec)
  in

  let pid_ref = ref None in
  let running = ref false in

  let on_timeout () =
    if !running then (
      let min, sec = runtime () in
      Printf.eprintf "%s%s test %s: %s (time: %02dm:%02ds)\n" error_prefix
        action colorized_test colorized_timeout min sec;
      (match !pid_ref with Some p -> Unix.kill p Sys.sigkill | None -> ());
      print_log ();
      cleanup ();
      exit 1)
  in

  ignore
    (Thread.create
       (fun () ->
         Unix.sleep timeout;
         on_timeout ())
       ());

  let pid = Unix.create_process cmd args stdin stdout stdout in
  running := true;
  pid_ref := Some pid;

  match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 ->
        running := false;
        let min, sec = runtime () in
        Printf.eprintf "%s test %s: %s (time: %02dm:%02ds)\n" action
          colorized_test colorized_ok min sec;
        if Sys.getenv_opt "LIQ_VERBOSE_TEST" <> None then print_log ();
        cleanup ()
    | _, Unix.WEXITED 123 ->
        running := false;
        Printf.eprintf "%s%s test %s: %s\n" warning_prefix action colorized_test
          colorized_skipped;
        exit 0
    | _ ->
        running := false;
        let min, sec = runtime () in
        Printf.eprintf "%s%s test %s: %s (time: %02dm:%02ds)\n" error_prefix
          action colorized_test colorized_failed min sec;
        print_log ();
        exit 1

let run () =
  (*
  Unix.putenv "MEMTRACE" (Printf.sprintf "%s.trace" test);
*)
  if String.starts_with ~prefix:"liquidsoap" cmd then
    run_process ~action:"Cached" cmd
      (Array.concat
         [
           [| args.(0) |];
           [| "--cache-only" |];
           Array.sub args 1 (Array.length args - 1);
         ]);

  run_process ~action:"Ran" cmd args

let () =
  try run ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    cleanup ();
    Printexc.raise_with_backtrace exn bt
