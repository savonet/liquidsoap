(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
module Doc = Liquidsoap_lang.Doc
module Environment = Liquidsoap_lang.Environment
module Profiler = Liquidsoap_lang.Profiler

let usage =
  {|Usage : liquidsoap [OPTION, SCRIPT or EXPR]...
 - SCRIPT for evaluating a liquidsoap script file,
 - EXPR   for evaluating a scripting expression,
 - OPTION is one of the options listed below:|}

let () =
  Configure.conf#plug "init" Dtools.Init.conf;
  Configure.conf#plug "log" Dtools.Log.conf

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
    ~p:(Dtools.Init.conf#plug "allow_root")
    ~d:false "Allow liquidsoap to run as root"
    ~comments:
      [
        "This should be reserved for advanced dynamic uses of liquidsoap ";
        "such as running inside an isolated environment like docker.";
      ]

(* Do not run, don't even check the scripts. *)
let parse_only = ref false

(* Should we load the stdlib? *)
let stdlib = ref true

(* Should we error if stdlib is not found? *)
let error_on_no_stdlib = not (Filename.is_relative Sys.argv.(0))

(* Should we load the deprecated wrapper? *)
let deprecated = ref true

(* Shall we start an interactive interpreter (REPL) *)
let interactive = ref false
let log = Log.make ["main"]

(** [load_libs] should be called before loading a script or looking up the
    documentation, to make sure that pervasive libraries have been loaded,
    unless the user explicitly opposed to it. *)
let load_libs =
  (* Register settings module. Needs to be done last to make sure every
     dependent OCaml module has been linked. *)
  Lazy.force Builtins_settings.settings_module;
  let loaded = ref false in
  fun () ->
    if !stdlib && not !loaded then (
      try
        let t = Sys.time () in
        Runtime.load_libs ~error_on_no_stdlib ~deprecated:!deprecated
          ~parse_only:!parse_only ();
        log#important "Standard library loaded in %.02f seconds."
          (Sys.time () -. t);
        loaded := true
      with Liquidsoap_lang.Runtime.Error ->
        flush_all ();
        exit 1)

let last_item_lib = ref false

(** Evaluate a script or expression.

    This used to be done immediately, which made it possible to write things
    like "liquidsoap myscript.liq -h myop" and get some doc on an operator.

    Now, we delay each evaluation because the last item has to be treated as a
    non-library, ie. all defined variables should be used. By default the last
    item is (the only one) not treated as a library, for better diagnosis, but
    this can be disabled (which is useful when --checking a lib).
*)
let do_eval, eval =
  let delayed = ref None in
  let eval src ~lib =
    try
      load_libs ();
      match src with
        | `StdIn -> Runtime.from_in_channel ~lib ~parse_only:!parse_only stdin
        | `Expr_or_File expr when not (Sys.file_exists expr) ->
            Runtime.from_string ~lib ~parse_only:!parse_only expr
        | `Expr_or_File f ->
            let basename = Filename.basename f in
            let basename =
              try Filename.chop_extension basename with _ -> basename
            in
            Utils.var_script := basename;
            Startup.time ("Loaded " ^ f) (fun () ->
                Runtime.from_file ~lib ~parse_only:!parse_only f)
    with Liquidsoap_lang.Runtime.Error ->
      flush_all ();
      exit 1
  in
  let force ~lib =
    match !delayed with
      | Some f ->
          f ~lib;
          delayed := None
      | None -> ()
  in
  ( force,
    fun src ->
      force ~lib:true;
      delayed := Some (eval src) )

let load_libs () =
  do_eval ~lib:true;
  load_libs ()

let lang_doc name =
  run_streams := false;
  load_libs ();
  try Lang_string.kprint_string ~pager:true (Doc.Value.print name)
  with Not_found -> Printf.printf "Plugin not found!\n%!"

let process_request s =
  load_libs ();
  run_streams := false;
  let req = Request.create s in
  match Request.resolve ~ctype:None req 20. with
    | Request.Failed ->
        Printf.eprintf "Request resolution failed.\n";
        Request.destroy req;
        flush_all ();
        exit 2
    | Request.Timeout ->
        Printf.eprintf "Request resolution timeout.\n";
        Request.destroy req;
        flush_all ();
        exit 1
    | Request.Resolved ->
        Request.read_metadata req;
        let metadata =
          Request.get_all_metadata req
          |> Hashtbl.to_seq
          |> Seq.map (fun (k, v) ->
                 (k, if String.length v > 1024 then "<redacted>" else v))
          |> Seq.map (fun (k, v) -> k ^ " = " ^ v)
          |> List.of_seq |> String.concat "\n"
        in
        Printf.printf "%s\n" metadata;
        Printf.printf "duration = %!";
        begin
          try
            Printf.printf "%.2f s\n"
              (Request.duration (Option.get (Request.get_filename req)))
          with Not_found -> Printf.printf "failed\n"
        end;
        Request.destroy req

let format_doc s =
  let prefix = "\t  " in
  let indent = 8 + 2 in
  let max_width = 80 in
  let s = Pcre.split ~pat:" " s in
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
         Arg.Unit (fun () -> eval `StdIn),
         "Read script from standard input." );
       ( ["-r"; "--request"],
         Arg.String process_request,
         "Process a file request and print the metadata." );
       ( ["-h"],
         Arg.String lang_doc,
         "Get help about a scripting value: source, operator, builtin or \
          library function, etc." );
       ( ["-c"; "--check"],
         Arg.Unit (fun () -> run_streams := false),
         "Check and evaluate scripts but do not perform any streaming." );
       ( ["-cl"; "--check-lib"],
         Arg.Unit
           (fun () ->
             last_item_lib := true;
             run_streams := false),
         "Like --check but treats all scripts and expressions as libraries, so \
          that unused toplevel variables are not reported." );
       ( ["-p"; "--parse-only"],
         Arg.Unit
           (fun () ->
             run_streams := false;
             parse_only := true),
         "Parse scripts but do not type-check and run them." );
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
              run_streams := false;
              load_libs ();
              Lang_string.kprint_string ~pager:true Doc.Plug.print_string),
          Printf.sprintf
            "List all plugins (builtin scripting values, supported formats and \
             protocols)." );
        ( ["--list-functions"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Lang_string.kprint_string ~pager:true Doc.Value.print_functions),
          Printf.sprintf "List all functions." );
        ( ["--list-functions-by-category"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Lang_string.kprint_string ~pager:true
                Doc.Value.print_functions_by_category),
          Printf.sprintf "List all functions, sorted by category." );
        ( ["--list-functions-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Lang_string.kprint_string ~pager:true
                (Doc.Value.print_functions_md ~extra:false)),
          Printf.sprintf "Documentation of all functions in markdown format." );
        ( ["--list-functions-json"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Lang_string.print_string ~pager:true
                (Json.to_string ~compact:false (Doc.Value.to_json ()))),
          Printf.sprintf "Documentation of all functions in JSON format." );
        ( ["--list-extra-functions-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Lang_string.kprint_string ~pager:true
                (Doc.Value.print_functions_md ~extra:true)),
          Printf.sprintf "Documentation of all extra functions in markdown." );
        ( ["--list-deprecated-functions-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              deprecated := true;
              load_libs ();
              Lang_string.kprint_string ~pager:true
                (Doc.Value.print_functions_md ~deprecated:true)),
          Printf.sprintf
            "Documentation of all deprecated functions in markdown." );
        ( ["--list-protocols-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Lang_string.kprint_string ~pager:true Doc.Protocol.print_md),
          Printf.sprintf "Documentation of all protocols in markdown." );
        ( ["--no-stdlib"],
          Arg.Clear stdlib,
          Printf.sprintf "Do not load stdlib script libraries (i.e., %s/*.liq)."
            (Configure.liq_libs_dir ()) );
        ( ["--no-deprecated"],
          Arg.Clear deprecated,
          "Do not load wrappers for deprecated operators." );
        ( ["-i"],
          Arg.Set Liquidsoap_lang.Typechecking.display_types,
          "Display inferred types." );
        ( ["--version"],
          Arg.Unit
            (fun () ->
              Printf.printf
                {|Liquidsoap %s
Copyright (c) 2003-2023 Savonet team
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
          Arg.Set interactive,
          "Start an interactive interpreter." );
        ( ["--"],
          Arg.Unit (fun () -> Arg.current := Array.length Shebang.argv - 1),
          "Stop parsing the command-line and pass subsequent items to the \
           script." );
        ( ["--list-settings"],
          Arg.Unit
            (fun () ->
              load_libs ();
              Lang_string.print_string ~pager:true
                (Builtins_settings.print_settings ());
              exit 0),
          "Display configuration keys in markdown format." );
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

let absolute s =
  if String.length s > 0 && s.[0] <> '/' then Sys.getcwd () ^ "/" ^ s else s

let () =
  (* Startup *)
  Lifecycle.before_init (fun () ->
      Random.self_init ();

      (* Set the default values. *)
      Dtools.Log.conf_file_path#set_d (Some "<syslogdir>/<script>.log");
      Dtools.Init.conf_daemon_pidfile#set_d (Some true);
      Dtools.Init.conf_daemon_pidfile_path#set_d
        (Some "<sysrundir>/<script>.pid");

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

  Lifecycle.on_script_parse (fun () ->
      (* Parse command-line, and notably load scripts. *)
      parse Shebang.argv (expand_options !options)
        (fun s -> eval (`Expr_or_File s))
        usage;

      if !interactive then (
        let default_log =
          Filename.temp_file
            (Printf.sprintf "liquidsoap-%d-" (Unix.getpid ()))
            ".log"
        in
        Dtools.Log.conf_file_path#set_d (Some default_log);
        Dtools.Log.conf_file#set true;
        Dtools.Log.conf_stdout#set false;
        Lifecycle.on_core_shutdown (fun _ -> Sys.remove default_log));

      (* Allow frame settings to be evaluated here: *)
      Frame_settings.lazy_config_eval := true;

      do_eval ~lib:!last_item_lib)

let initial_cleanup () =
  if !Term.profile then
    log#important "Profiler stats:\n\n%s" (Profiler.stats ());
  Clock.stop ();
  if Tutils.has_started () then (
    log#important "Waiting for main threads to terminate...";
    Tutils.join_all ();
    log#important "Main threads terminated.")

let final_cleanup () =
  log#important "Cleaning downloaded files...";
  Request.clean ();
  log#important "Freeing memory...";
  Gc.full_major ();
  Gc.full_major ()

let sync_cleanup () =
  initial_cleanup ();
  final_cleanup ()

let () =
  (* Shutdown *)
  Lifecycle.before_core_shutdown (fun () -> log#important "Shutdown started!");

  Lifecycle.on_core_shutdown initial_cleanup;

  Lifecycle.on_final_cleanup final_cleanup;

  Lifecycle.after_stop (fun () ->
      let code = Tutils.exit_code () in
      if code <> 0 then exit code;
      if !Configure.restart then (
        log#important "Restarting...";
        Unix.execv Sys.executable_name Sys.argv))

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
  %s.set("<path>")|}
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
  Lifecycle.on_start (fun () ->
      let main () =
        (* See http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4640 for
           this: we want Unix EPIPE error and not SIGPIPE, which crashes the
           program... *)
        if not Sys.win32 then (
          Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
          ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe]));

        (* On Windows we need to initiate shutdown ourselves by catching INT
           since dtools doesn't do it. *)
        if Sys.win32 then
          Sys.set_signal Sys.sigint
            (Sys.Signal_handle (fun _ -> Tutils.shutdown 0));

        Dtools.Init.exec Dtools.Log.start;

        (* TODO: if start fails (e.g. invalid password or mountpoint) it raises
           an exception and dtools catches it so we don't get a backtrace (by
           default at least). *)
        Server.start ();
        Clock.start ();
        Tutils.start ();
        Tutils.main ()
      in
      if !run_streams then
        if !interactive then (
          load_libs ();
          check_directories ();
          ignore
            (Thread.create
               (fun () ->
                 Runtime.interactive ();
                 Tutils.shutdown 0)
               ());
          Dtools.Init.init main)
        else if Source.has_outputs () || force_start#get then (
          check_directories ();
          let msg_of_err = function
            | `User -> "root euid (user)"
            | `Group -> "root guid (group)"
            | `Both -> "root euid & guid (user & group)"
          in
          let on_error e =
            Printf.eprintf
              "init: security exit, %s. Override with \
               settings.init.allow_root.set(true)\n"
              (msg_of_err e);
            sync_cleanup ();
            exit (-1)
          in
          try Dtools.Init.init ~prohibit_root:(not allow_root#get) main
          with Dtools.Init.Root_prohibited e -> on_error e)
        else (
          final_cleanup ();
          Printf.printf "No output defined, nothing to do.\n";
          flush_all ();
          exit 1))

(* Here we go! *)
let start () = Dtools.Init.exec Lifecycle.init_atom
