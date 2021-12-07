(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let usage =
  "Usage : liquidsoap [OPTION, SCRIPT or EXPR]...\n\
  \ - SCRIPT for evaluating a liquidsoap script file;\n\
  \ - EXPR for evaluating a scripting expression;\n\
  \ - OPTION is one of the options listed below:\n"

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

(** [load_libs] should be called before loading a script or looking up
  * the documentation, to make sure that pervasive libraries have been
  * loaded, unless the user explicitly opposed to it. *)
let load_libs =
  (* Register settings module. Needs to be done last to make sure every
     dependent OCaml module has been linked. *)
  Lazy.force Builtins_settings.settings_module;
  let loaded = ref false in
  fun () ->
    if !stdlib && not !loaded then (
      let save = !Configure.display_types in
      Configure.display_types := false;
      Runtime.load_libs ~error_on_no_stdlib ~deprecated:!deprecated
        ~parse_only:!parse_only ();
      loaded := true;
      Configure.display_types := save)

(** Evaluate a script or expression.
  * This used to be done immediately, which made it possible to
  * write things like "liquidsoap myscript.liq -h myop" and get
  * some doc on an operator.
  * Now, we delay each evaluation because the last item has to be treated
  * as a non-library, ie. all defined variables should be used.
  * By default the last item is (the only one) not treated as a library,
  * for better diagnosis, but this can be disabled (which is useful
  * when --checking a lib). *)

let last_item_lib = ref false

let do_eval, eval =
  let delayed = ref None in
  let eval src ~lib =
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
          Configure.var_script := basename;
          Runtime.from_file ~lib ~parse_only:!parse_only f
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
  try Doc.print_lang (Environment.builtins#get_subsection name)
  with Not_found -> Printf.printf "Plugin not found!\n%!"

let process_request s =
  load_libs ();
  run_streams := false;
  let req = Request.create s in
  match Request.resolve ~ctype:None req 20. with
    | Request.Failed ->
        Printf.printf "Request resolution failed.\n";
        Request.destroy req;
        exit 2
    | Request.Timeout ->
        Printf.printf "Request resolution timeout.\n";
        Request.destroy req;
        exit 1
    | Request.Resolved ->
        let metadata = Request.get_all_metadata req in
        let metadata = Request.string_of_metadata metadata in
        Printf.printf "Request resolved.\n%s\n" metadata;
        Printf.printf "Computing duration: %!";
        begin
          try
            Printf.printf "%.2f sec.\n"
              (Request.duration (Option.get (Request.get_filename req)))
          with Not_found -> Printf.printf "failed.\n"
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

let log = Log.make ["main"]

let options =
  ref
    ([
       ( ["-"],
         Arg.Unit (fun () -> eval `StdIn),
         "Read script from standard input." );
       (["-r"], Arg.String process_request, "Process a request.");
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
         Arg.Unit (fun () -> Term.conf_debug_errors#set true),
         "Debug errors (show stacktrace instead of printing a message)." );
       ( ["--debug-lang"],
         Arg.Unit
           (fun () ->
             Type.debug := true;
             Term.conf_debug#set true),
         "Debug language implementation." );
       (["--profile"], Arg.Set Term.profile, "Profile execution.");
       ( ["--strict"],
         Arg.Set Runtime.strict,
         "Execute script code in strict mode, issuing fatal errors instead of \
          warnings in some cases. Currently: unused variables and ignored \
          expressions. " );
     ]
    @ Dtools.Init.args
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
        ( ["--list-plugins-xml"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print_xml (Plug.plugs : Doc.item))),
          Printf.sprintf
            "List all plugins (builtin scripting values, supported formats and \
             protocols), output as XML." );
        ( ["--list-plugins-json"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print_json (Plug.plugs : Doc.item))),
          Printf.sprintf
            "List all plugins (builtin scripting values, supported formats and \
             protocols), output as JSON." );
        ( ["--list-plugins"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print (Plug.plugs : Doc.item))),
          Printf.sprintf
            "List all plugins (builtin scripting values, supported formats and \
             protocols)." );
        ( ["--list-functions"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print_functions (Plug.plugs : Doc.item))),
          Printf.sprintf "List all functions." );
        ( ["--list-functions-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print_functions_md ~extra:false (Plug.plugs : Doc.item))),
          Printf.sprintf "Documentation of all functions in markdown." );
        ( ["--list-extra-functions-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print_functions_md ~extra:true (Plug.plugs : Doc.item))),
          Printf.sprintf "Documentation of all extra functions in markdown." );
        ( ["--list-protocols-md"],
          Arg.Unit
            (fun () ->
              run_streams := false;
              load_libs ();
              Utils.kprint_string ~pager:true
                (Doc.print_protocols_md (Plug.plugs : Doc.item))),
          Printf.sprintf "Documentation of all protocols in markdown." );
        ( ["--no-stdlib"],
          Arg.Clear stdlib,
          Printf.sprintf "Do not load stdlib script libraries (i.e., %s/*.liq)."
            Configure.liq_libs_dir );
        ( ["--no-deprecated"],
          Arg.Clear deprecated,
          "Do not load wrappers for deprecated operators." );
        (["-i"], Arg.Set Configure.display_types, "Display inferred types.");
        ( ["--version"],
          Arg.Unit
            (fun () ->
              Printf.printf
                "Liquidsoap %s\n\
                 Copyright (c) 2003-2021 Savonet team\n\
                 Liquidsoap is open-source software, released under GNU \
                 General Public License.\n\
                 See <http://liquidsoap.info> for more information.\n"
                Configure.version;
              exit 0),
          "Display liquidsoap's version." );
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
              Utils.print_string ~pager:true
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
        Utils.print_string ~pager:true msg;
        exit 0

let absolute s =
  if String.length s > 0 && s.[0] <> '/' then Unix.getcwd () ^ "/" ^ s else s

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
      log#important "Using:%s" Configure.libs_versions;
      if Configure.git_snapshot then
        List.iter (log#important "%s")
          [
            "";
            "DISCLAIMER: This version of Liquidsoap has been";
            "compiled from a snapshot of the development code.";
            "As such, it should not be used in production";
            "unless you know what you are doing!";
            "";
            "We are, however, very interested in any feedback";
            "about our development code and committed to fix";
            "issues as soon as possible.";
            "";
            "If you are interested in collaborating to";
            "the development of Liquidsoap, feel free to";
            "drop us a mail at <savonet-devl@lists.sf.net>";
            "or to join the slack chat at <http://slack.liquidsoap.info>.";
            "";
            "Please send any bug report or feature request";
            "at <https://github.com/savonet/liquidsoap/issues>.";
            "";
            "We hope you enjoy this snapshot build of Liquidsoap!";
            "";
          ]);

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
      Frame_base.lazy_config_eval := true;

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

(* When the log/pid paths have their definitive values,
 * expand substitutions and check directories.
 * This should be ran just before Dtools init. *)
let check_directories () =
  (* Now that the paths have their definitive value, expand <shortcuts>. *)
  let subst conf = conf#set (Configure.subst_vars conf#get) in
  subst Dtools.Init.conf_daemon_pidfile_path;
  let check_dir conf_path kind =
    let path = conf_path#get in
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then (
      let routes = Configure.conf#routes conf_path#ut in
      Printf.printf
        "FATAL ERROR: %s directory %S does not exist.\n\
         To change it, add the following to your script:\n\
        \  set(%S, \"<path>\")\n"
        kind dir
        (Dtools.Conf.string_of_path (List.hd routes));
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
        (* See http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4640
         * for this: we want Unix EPIPE error and not SIGPIPE, which
         * crashes the program.. *)
        if not Sys.win32 then (
          Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
          ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe]));

        (* On Windows we need to initiate shutdown ourselves by catching INT
         * since dtools doesn't do it. *)
        if Sys.win32 then
          Sys.set_signal Sys.sigint
            (Sys.Signal_handle (fun _ -> Tutils.shutdown 0));

        Dtools.Init.exec Dtools.Log.start;

        (* TODO if start fails (e.g. invalid password or mountpoint) it
         *   raises an exception and dtools catches it so we don't get
         *   a backtrace (by default at least). *)
        Clock.start ();
        Tutils.start ();
        Tutils.main ()
      in
      if !run_streams then
        if !interactive then (
          load_libs ();
          check_directories ();
          ignore (Thread.create Runtime.interactive ());
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
               set(\"init.allow_root\",true)\n"
              (msg_of_err e);
            sync_cleanup ();
            exit (-1)
          in
          try Dtools.Init.init ~prohibit_root:(not allow_root#get) main
          with Dtools.Init.Root_prohibited e -> on_error e)
        else (
          final_cleanup ();
          Printf.printf "No output defined, nothing to do.\n";
          exit 1))

(* Here we go! *)
let start () = Dtools.Init.exec Lifecycle.init_atom
