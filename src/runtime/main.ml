(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

module Runtime = Liquidsoap_lang.Runtime
module Environment = Liquidsoap_lang.Environment
module Profiler = Liquidsoap_lang.Profiler
module Lang_string = Liquidsoap_lang.Lang_string
module Queue = Queues.Queue

let usage =
  {|Usage : liquidsoap [OPTION, SCRIPT or EXPR]...
 - SCRIPT for evaluating a liquidsoap script file,
 - EXPR   for evaluating a scripting expression,
 - OPTION is one of the options listed below:|}

(* Set log to stdout by default. *)
let () =
  Dtools.Log.conf_stdout#set_d (Some true);
  Dtools.Log.conf_file#set_d (Some false);
  Dtools.Log.conf_file_path#on_change (fun _ ->
      Dtools.Log.conf_stdout#set_d (Some false);
      Dtools.Log.conf_file#set_d (Some true))

(* Are we streaming? *)
let run_streams = ref true

(* Should we start even without active sources? *)
let force_start =
  Dtools.Conf.bool
    ~p:(Dtools.Init.conf#plug "force_start")
    ~d:false "Start liquidsoap even without any active source"
    ~comments:
      ["This should be reserved for advanced dynamic uses of liquidsoap."]

(* Should we allow to run as root? *)
let allow_root =
  Dtools.Conf.bool
    ~p:(Configure.conf_init#plug "allow_root")
    ~d:(Lazy.force Utils.is_docker)
    "Allow liquidsoap to run as root"
    ~comments:
      [
        "This should be reserved for advanced dynamic uses of liquidsoap ";
        "such as running inside an isolated environment like docker.";
      ]

let root_error () =
  match (allow_root#get, Unix.geteuid (), Unix.getegid ()) with
    | false, 0, 0 -> Some "root euid & guid (user & group)"
    | false, 0, _ -> Some "root euid (user)"
    | false, _, 0 -> Some "root guid (group)"
    | _ -> None

let eval_mode : [ `Parse_only | `Parse_and_type | `Eval | `Eval_toplevel ] ref =
  ref `Eval

let print_json_term = ref false

(* Should we error if stdlib is not found? *)
let is_relative = Filename.is_relative Sys.argv.(0)

(* Should we load the stdlib? *)
let stdlib : Liquidsoap_lang.Lang_eval.stdlib ref =
  ref (if is_relative then `If_present else `Force)

(* Shall we use the cache *)
let cache = ref true

(* Display cache key. *)
let show_cache_key = ref false
let deprecated = Liquidsoap_lang.Runtime.deprecated

(* Shall we start an interactive interpreter (REPL) *)
let interactive = ref false
let log = Log.make ["main"]
let to_load = Queue.create ()

let eval_script expr =
  let open Liquidsoap_lang in
  match !eval_mode with
    | `Parse_only ->
        let tm, _ = Runtime.parse expr in
        if !print_json_term then
          Printf.printf "%s\n"
            (Liquidsoap_lang.Json.to_string ~compact:false
               (Liquidsoap_tooling.Parsed_json.to_json tm))
    | `Parse_and_type ->
        let parsed_term, term = Runtime.parse expr in
        ignore
          (Lang.type_term ~name:"main script" ~parsed_term ~stdlib:!stdlib
             ~trim:true ~deprecated:!deprecated term);
        if !show_cache_key then
          Printf.printf "Term cached with key %s\n"
            (Parsed_term.hash parsed_term)
    | (`Eval as v) | (`Eval_toplevel as v) ->
        let toplevel = v = `Eval_toplevel in
        let stdlib = !stdlib in
        ignore
          (Lang.eval ~toplevel ~cache:!cache ~stdlib ~deprecated:!deprecated
             ~name:"main script" expr);
        if not (Lang_eval.effective_toplevel ~stdlib toplevel) then
          Environment.clear_toplevel_environments ()

(** Evaluate the user script. *)
let eval () =
  Lifecycle.load ();
  (* Register settings module. Needs to be done last to make sure every
     dependent OCaml module has been linked. *)
  Stdlib.Lazy.force Builtins_settings.settings_module;
  let scripts = Queue.flush_elements to_load in
  let script =
    String.concat "\n"
      (List.map
         (function
           | `Stdin -> "%include \"-\""
           | `Expr_or_file expr when not (Sys.file_exists expr) ->
               Printf.sprintf "%s" expr
           | `Expr_or_file f ->
               Printf.sprintf "%%include %s" (Lang_string.quote_string f))
         scripts)
  in
  let t = Sys.time () in
  try
    eval_script script;
    log#important "User script loaded in %.02f seconds." (Sys.time () -. t)
  with Liquidsoap_lang.Runtime.Error ->
    Dtools.Init.exec Dtools.Log.stop;
    flush_all ();
    exit 1

let with_toplevel =
  let do_run_streams = run_streams in
  let do_exit = exit in
  fun ?(exit = true) ?(run_streams = false) fn ->
    do_run_streams := run_streams;
    eval_mode := `Eval_toplevel;
    eval ();
    fn ();
    if exit then do_exit 0

let lang_doc name =
  with_toplevel (fun () ->
      try Lang_string.kprint_string ~pager:true (Doc.Value.print name)
      with Not_found -> Printf.printf "Plugin not found!\n%!")

let process_request s =
  with_toplevel (fun () ->
      let req = Request.create ~cue_in_metadata:None ~cue_out_metadata:None s in
      match Request.resolve req with
        | `Failed ->
            Printf.eprintf "Request resolution failed.\n";
            Request.destroy req;
            flush_all ();
            exit 2
        | `Timeout ->
            Printf.eprintf "Request resolution timeout.\n";
            Request.destroy req;
            flush_all ();
            exit 1
        | `Resolved ->
            let metadata =
              Request.metadata req |> Frame.Metadata.to_list |> List.to_seq
              |> Seq.map (fun (k, v) ->
                     (k, if String.length v > 1024 then "<redacted>" else v))
              |> Seq.map (fun (k, v) -> k ^ " = " ^ v)
              |> List.of_seq |> String.concat "\n"
            in
            Printf.printf "%s\n" metadata;
            Printf.printf "duration = %!";
            begin
              match
                Request.duration ~metadata:(Request.metadata req)
                  (Option.get (Request.get_filename req))
              with
                | Some f -> Printf.printf "%.2f s\n" f
                | None -> Printf.printf "n/a\n"
                | exception Not_found -> Printf.printf "failed\n"
            end;
            Request.destroy req)

let format_doc s =
  let prefix = "\t  " in
  let indent = 8 + 2 in
  let max_width = 80 in
  let s = String.split_on_char ' ' s in
  let s =
    let rec join line width = function
      | [] -> [line]
      | hd :: tl ->
          let hdw = String.length hd in
          let w = width + 1 + hdw in
          if w < max_width then join (line ^ " " ^ hd) w tl
          else line :: join (prefix ^ hd) hdw tl
    in
    match s with
      | hd :: tl -> join (prefix ^ hd) (indent + String.length hd) tl
      | [] -> []
  in
  String.concat "\n" s

let options =
  ref
    ([
       ( ["-"],
         Arg.Unit (fun () -> Queue.push to_load `Stdin),
         "Read script from standard input." );
       ( ["-r"; "--request"],
         Arg.String process_request,
         "Process a file request and print the metadata." );
       ( ["-h"],
         Arg.String lang_doc,
         "Get help about a scripting value: source, operator, builtin or \
          library function, etc." );
       ( ["-c"; "--check"],
         Arg.Unit
           (fun () ->
             run_streams := false;
             eval_mode := `Parse_and_type),
         "Parse, type-check but do not evaluate the script." );
       ( ["-p"; "--parse-only"],
         Arg.Unit (fun () -> eval_mode := `Parse_only),
         "Parse script but do not type-check and run them." );
       ( ["--cache-stdlib"],
         Arg.Unit (fun () -> with_toplevel (fun () -> ())),
         "Generate the standard library cache." );
       ( ["--cache-only"],
         Arg.Unit
           (fun () ->
             run_streams := false;
             eval_mode := `Parse_and_type;
             show_cache_key := true;
             cache := true),
         "Parse, type-check and save script's cache but do no run it." );
       (["--no-cache"], Arg.Clear cache, "Disable cache");
       ( ["--no-external-plugins"],
         Arg.Clear Startup.register_external_plugins,
         "Disable external plugins." );
       ( ["-q"; "--quiet"],
         Arg.Unit (fun () -> Dtools.Log.conf_stdout#set false),
         "Do not print log messages on standard output." );
       ( ["-v"; "--verbose"],
         Arg.Unit (fun () -> Dtools.Log.conf_stdout#set true),
         "Print log messages on standard output." );
       ( ["-f"; "--force-start"],
         Arg.Unit (fun () -> force_start#set true),
         "For advanced dynamic uses: force liquidsoap to start even when no \
          active source is initially defined." );
       ( ["--debug"],
         Arg.Unit
           (fun () ->
             Dtools.Log.conf_level#set (max 4 Dtools.Log.conf_level#get)),
         "Print debugging log messages." );
       ( ["--debug-errors"],
         Arg.Unit (fun () -> Configure.conf_debug_errors#set true),
         "Debug errors (show stacktrace instead of printing a message)." );
       ( ["--debug-subtyping"],
         Arg.Set Typing.debug_subtyping,
         "Debug subtyping." );
       ( ["--debug-lang"],
         Arg.Unit
           (fun () ->
             Type.debug := true;
             Configure.conf_debug#set true),
         "Debug language implementation." );
       ( ["--debug-levels"],
         Arg.Unit (fun () -> Type.debug_levels := true),
         "Debug typing levels." );
       (["--profile"], Arg.Set Term.profile, "Profile execution.");
       ( ["--strict"],
         Arg.Set Runtime.strict,
         "Execute script code in strict mode, issuing fatal errors instead of \
          warnings in some cases. Currently: unused variables and ignored \
          expressions. " );
       ( ["--raw-errors"],
         Arg.Set Runtime.raw_errors,
         "In normal executions, exceptions raised during the script are \
          translated into user-friendly errors. Use this option to let the \
          original error surface. This is useful when debugging." );
     ]
    @ Dtools.Init.args @ Extra_args.args ()
    @ [
        ( ["-t"; "--enable-telnet"],
          Arg.Unit (fun _ -> Server.conf_telnet#set true),
          "Enable the telnet server." );
        ( ["-T"; "--disable-telnet"],
          Arg.Unit (fun _ -> Server.conf_telnet#set false),
          "Disable the telnet server." );
        ( ["-u"; "--enable-unix-socket"],
          Arg.Unit (fun _ -> Server.conf_socket#set true),
          "Enable the unix socket." );
        ( ["-U"; "--disable-unix-socket"],
          Arg.Unit (fun _ -> Server.conf_socket#set false),
          "Disable the unix socket." );
        ( ["--list-plugins"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true Doc.Plug.print_string)),
          Printf.sprintf
            "List all plugins (builtin scripting values, supported formats and \
             protocols)." );
        ( ["--list-functions"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true
                    Doc.Value.print_functions)),
          Printf.sprintf "List all functions." );
        ( ["--list-functions-by-category"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true
                    Doc.Value.print_functions_by_category)),
          Printf.sprintf "List all functions, sorted by category." );
        ( ["--list-functions-md"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true
                    (Doc.Value.print_functions_md ~extra:false))),
          Printf.sprintf "Documentation of all functions in markdown format." );
        ( ["--list-functions-json"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.print_string ~pager:true
                    (Json.to_string ~compact:false (Doc.Value.to_json ())))),
          Printf.sprintf "Documentation of all functions in JSON format." );
        ( ["--list-extra-functions-md"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true
                    (Doc.Value.print_functions_md ~extra:true))),
          Printf.sprintf "Documentation of all extra functions in markdown." );
        ( ["--list-deprecated-functions-md"],
          Arg.Unit
            (fun () ->
              deprecated := true;
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true
                    (Doc.Value.print_functions_md ~deprecated:true))),
          Printf.sprintf
            "Documentation of all deprecated functions in markdown." );
        ( ["--list-protocols-md"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.kprint_string ~pager:true Doc.Protocol.print_md)),
          Printf.sprintf "Documentation of all protocols in markdown." );
        ( ["--no-stdlib"],
          Arg.Unit (fun () -> stdlib := `Disabled),
          Printf.sprintf "Do not load stdlib script libraries (i.e., %s/*.liq)."
            (Configure.liq_libs_dir ()) );
        ( ["--print-json-term"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              eval_mode := `Parse_only;
              print_json_term := true),
          "Parse and output the script as normalized JSON. The JSON format is \
           used internally to format code." );
        ( ["--stdlib"],
          Arg.String (fun s -> stdlib := `Override s),
          "Override the location of the standard library." );
        ( ["--disable-deprecated"],
          Arg.Clear deprecated,
          "Do not load wrappers for deprecated operators." );
        ( ["--no-deprecated"],
          Arg.Unit
            (fun _ ->
              Printf.eprintf
                "`--no-deprecated` is, ahem.. deprecated! Please use \
                 `--disable-deprecated`!";
              deprecated := false),
          "Deprecated: use `--disable-deprecated`" );
        ( ["--enable-deprecated"],
          Arg.Set deprecated,
          "Load wrappers for deprecated operators." );
        ( ["-i"],
          Arg.Set Liquidsoap_lang.Typechecking.display_types,
          "Display inferred types." );
        ( ["--version"],
          Arg.Unit
            (fun () ->
              Printf.printf
                {|Liquidsoap %s
Copyright (c) 2003-2024 Savonet team
Liquidsoap is open-source software, released under GNU General Public License.
See <http://liquidsoap.info> for more information.
|}
                Configure.version;
              exit 0),
          "Display liquidsoap's version." );
        ( ["--build-config"],
          Arg.Unit
            (fun () ->
              Printf.printf "%s\n" Build_config.build_config;
              exit 0),
          "Display liquidsoap's build configuration." );
        ( ["--opam-config"],
          Arg.Unit
            (fun () ->
              Printf.printf "%s\n" Build_config.opam_config;
              exit 0),
          "Print out opam's liquidsoap.config, for internal use." );
        ( ["--interactive"],
          Arg.Unit
            (fun () ->
              interactive := true;
              eval_mode := `Eval_toplevel),
          "Start an interactive interpreter." );
        ( ["--"],
          Arg.Unit (fun () -> Arg.current := Array.length Shebang.argv - 1),
          "Stop parsing the command-line and pass subsequent items to the \
           script." );
        ( ["--list-settings"],
          Arg.Unit
            (fun () ->
              with_toplevel (fun () ->
                  Lang_string.print_string ~pager:true
                    (Builtins_settings.print_settings ()))),
          "Display configuration keys in markdown format." );
        ( ["--unsafe"],
          Arg.Unit (fun () -> Typing.do_occur_check := false),
          "Faster startup using unsafe features." );
        ( ["--safe"],
          Arg.Unit (fun () -> Typing.do_occur_check := true),
          "Disable the effects of --unsafe." );
        ( ["--no-fallible-check"],
          Arg.Unit (fun () -> Output.fallibility_check := false),
          "Ignore fallible sources." );
      ])

let expand_options options =
  let options = List.sort (fun (x, _, _) (y, _, _) -> compare x y) options in
  List.fold_left
    (fun l (la, b, c) ->
      let ta = List.hd (List.rev la) in
      let expand =
        List.map
          (fun a -> (a, b, if a = ta then "\n" ^ format_doc c else " "))
          la
      in
      l @ expand)
    [] options

(** Just like Arg.parse_argv but with Arg.parse's behavior on errors.. *)
let parse argv l f msg =
  try Arg.parse_argv argv l f msg with
    | Arg.Bad msg ->
        Printf.eprintf "%s" msg;
        exit 2
    | Arg.Help msg ->
        Lang_string.print_string ~pager:true msg;
        exit 0

let parse_options () =
  (* Parse command-line, and notably load scripts. *)
  parse Shebang.argv (expand_options !options)
    (fun s -> Queue.push to_load (`Expr_or_file s))
    usage

let absolute s =
  if String.length s > 0 && s.[0] <> '/' then Sys.getcwd () ^ "/" ^ s else s

let () =
  (* Startup *)
  Lifecycle.before_init ~name:"main application init" (fun () ->
      Random.self_init ();

      (* Set the default values. *)
      Dtools.Log.conf_file_path#set_d (Some "<syslogdir>/<script>.log");
      Dtools.Init.conf_daemon_pidfile#set_d (Some true);
      Dtools.Init.conf_daemon_pidfile_path#set_d
        (Some "<sysrundir>/<script>.pid");

      Utils.add_subst "<sysrundir>" (Liquidsoap_paths.rundir ());
      Utils.add_subst "<syslogdir>" (Liquidsoap_paths.logdir ());

      log#important "Liquidsoap %s" Configure.version;
      log#important "Using: %s" (Configure.libs_versions ());
      if Configure.git_snapshot then
        List.iter (log#important "%s")
          ({|
DISCLAIMER: This version of Liquidsoap has been compiled from a snapshot of the
development code. As such, it should not be used in production unless you know
what you are doing!

We are, however, very interested in any feedback about our development code and
committed to fix issues as soon as possible.

If you are interested in collaborating to the development of Liquidsoap, feel
free to drop us a mail at <savonet-devl@lists.sf.net> or to join the slack chat
at <http://slack.liquidsoap.info>.

Please send any bug report or feature request at
<https://github.com/savonet/liquidsoap/issues>.

We hope you enjoy this snapshot build of Liquidsoap!
|}
         |> String.split_on_char '\n'));

  Lifecycle.on_script_parse ~name:"main application script parse" (fun () ->
      parse_options ();

      if !interactive then (
        let default_log =
          Filename.temp_file
            (Printf.sprintf "liquidsoap-%d-" (Unix.getpid ()))
            ".log"
        in
        Dtools.Log.conf_file_path#set_d (Some default_log);
        Dtools.Log.conf_file#set true;
        Dtools.Log.conf_stdout#set false;
        Lifecycle.after_core_shutdown ~name:"remove logs" (fun _ ->
            Sys.remove default_log));

      (* Allow frame settings to be evaluated here: *)
      Frame_settings.lazy_config_eval := true;

      eval ())

let initial_cleanup () =
  if !Term.profile then
    log#important "Profiler stats:\n\n%s" (Profiler.stats ());
  Tutils.cleanup ()

let final_cleanup () =
  log#important "Cleaning downloaded files...";
  Request.cleanup ();
  log#important "Freeing memory...";
  Dtools.Init.exec Dtools.Log.stop;
  flush_all ();
  Gc.full_major ();
  Gc.full_major ()

let sync_cleanup () =
  initial_cleanup ();
  final_cleanup ()

let () =
  (* Shutdown *)
  Lifecycle.before_core_shutdown ~name:"log shutdown" (fun () ->
      log#important "Shutdown started!");

  Lifecycle.on_core_shutdown ~name:"initial cleanup" initial_cleanup;

  Lifecycle.on_final_cleanup ~name:"final cleanup" final_cleanup;

  Lifecycle.after_final_cleanup ~name:"main application exit" (fun () ->
      match (Tutils.exit_code (), !Configure.restart) with
        | 0, true ->
            log#important "Restarting...";
            Unix.execv Sys.executable_name Sys.argv
        | _ ->
            flush_all ();
            Tutils.exit ())

(** Main procedure *)

(** When the log/pid paths have their definitive values, expand substitutions
    and check directories. This should be ran just before Dtools init. *)
let check_directories () =
  (* Now that the paths have their definitive value, expand <shortcuts>. *)
  let subst conf = conf#set (Utils.subst_vars conf#get) in
  subst Dtools.Init.conf_daemon_pidfile_path;
  let check_dir conf_path kind =
    let path = conf_path#get in
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then (
      let routes = Configure.conf#routes conf_path#ut in
      Printf.printf
        {|FATAL ERROR: %s directory %S does not exist.
To change it, add the following to your script:
  %s := "<path>"|}
        kind dir
        (Dtools.Conf.string_of_path (List.hd routes));
      flush_all ();
      exit 1)
  in
  if Dtools.Log.conf_file#get then (
    subst Dtools.Log.conf_file_path;
    check_dir Dtools.Log.conf_file_path "Log");
  if Dtools.Init.conf_daemon#get && Dtools.Init.conf_daemon_pidfile#get then
    check_dir Dtools.Init.conf_daemon_pidfile_path "PID"

let () =
  Dtools.Init.conf_daemon#on_change (fun v ->
      if v then
        log#important
          "Script-base daemonization is DEPRECATED! Please use a modern \
           daemonization facility such as `systemd` or `launchd` instead.")

let daemonize () =
  Dtools.Log.conf_stdout#set false;
  (* Change user.. *)
  let conf_daemon_change_user =
    Dtools.Conf.as_bool (Dtools.Init.conf_daemon#path ["change_user"])
  in
  let conf_daemon_user =
    Dtools.Conf.as_string (conf_daemon_change_user#path ["user"])
  in
  let conf_daemon_group =
    Dtools.Conf.as_string (conf_daemon_change_user#path ["group"])
  in
  if conf_daemon_change_user#get then begin
    let grd = Unix.getgrnam conf_daemon_group#get in
    let gid = grd.Unix.gr_gid in
    if Unix.getegid () <> gid then Unix.setgid gid;
    let pwd = Unix.getpwnam conf_daemon_user#get in
    let uid = pwd.Unix.pw_uid in
    if Unix.geteuid () <> uid then Unix.setuid uid
  end;
  if Unix.fork () <> 0 then exit 0;
  (* Detach from the console *)
  if Unix.setsid () < 0 then exit 1;
  (* Refork.. *)
  if Unix.fork () <> 0 then exit 0;
  (* Change umask to 0 *)
  ignore (Unix.umask 0);
  (* chdir to / *)
  Unix.chdir "/";
  if Dtools.Init.conf_daemon_pidfile#get then begin
    (* Write PID to file *)
    let filename = Dtools.Init.conf_daemon_pidfile_path#get in
    let f =
      open_out_gen
        [Open_wronly; Open_creat; Open_trunc]
        Dtools.Init.conf_daemon_pidfile_perms#get filename
    in
    let pid = Unix.getpid () in
    output_string f (string_of_int pid);
    output_char f '\n';
    close_out f
  end;
  (* Reopen usual file descriptor *)
  Utils.reopen_in stdin "/dev/null";
  Utils.reopen_out stdout "/dev/null";
  Utils.reopen_out stderr "/dev/null"

let () =
  Lifecycle.before_start ~name:"main application before start" (fun () ->
      if not !run_streams then (
        final_cleanup ();
        flush_all ();
        exit 0);

      if
        (not !interactive)
        && List.length (Clock.clocks ()) = 0
        && not force_start#get
      then (
        final_cleanup ();
        Printf.printf "No output defined, nothing to do.\n";
        flush_all ();
        exit 1);

      if Dtools.Init.conf_daemon#get then daemonize ();

      (match root_error () with
        | None -> ()
        | Some err ->
            Printf.eprintf
              "init: security exit, %s. Override with settings.init.allow_root \
               := true\n"
              err;
            sync_cleanup ();
            exit (-1));

      check_directories ();
      Dtools.Init.exec Dtools.Log.start);

  Lifecycle.on_start ~name:"main application start" (fun () ->
      (* See http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4640 for
         this: we want Unix EPIPE error and not SIGPIPE, which crashes the
         program... *)
      if not Sys.win32 then (
        Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
        ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe]));

      let sigcount = Atomic.make 0 in
      let sigterm_handler _ =
        Tutils.shutdown 0;
        match Atomic.fetch_and_add sigcount 1 with
          | 0 -> log#important "Shutdown signal received."
          | 1 ->
              log#severe
                "Shutdown signal received. Send another time for a hard \
                 shutdown."
          | 2 ->
              Printf.eprintf
                "\nShutdown signal received 3 times, shutting down..\n%!";
              exit 128
          | _ -> ()
      in

      if not Sys.win32 then
        Sys.set_signal Sys.sigterm (Sys.Signal_handle sigterm_handler);
      Sys.set_signal Sys.sigint (Sys.Signal_handle sigterm_handler);

      (* TODO: if start fails (e.g. invalid password or mountpoint) it raises
         an exception and dtools catches it so we don't get a backtrace (by
         default at least). *)
      Server.start ();
      Tutils.start ();

      if !interactive then
        ignore
          (Thread.create
             (fun () ->
               Runtime.interactive ();
               Tutils.shutdown 0)
             ()));
  Lifecycle.on_main_loop ~name:"main application main loop" Tutils.main

(* Here we go! *)
let start = Lifecycle.init
