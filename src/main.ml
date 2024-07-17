(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Root
open Dtools
open Printf

let usage =
  "Usage : liquidsoap [OPTION, SCRIPT or EXPR]...\n\
  \ - SCRIPT for evaluating a liquidsoap script file;\n\
  \ - EXPR for evaluating a scripting expression;\n\
  \ - OPTION is one of the options listed below:\n"

let () =
  Configure.conf#plug "init" Init.conf ;
  Configure.conf#plug "log" Log.conf

(* Should we not_run the outputs? *)
let dont_run = ref false

(* Do not run, don't even check the scripts. *)
let parse_only = ref false

(* Should we load the pervasives? *)
let pervasives = ref true

(* Have we been used for an other purpose than streaming? *)
let secondary_task = ref false

(** [check_pervasives] should be called before loading a script or looking up
  * the documentation, to make sure that pervasive libraries have been loaded,
  * unless the user explicitly opposed to it. *)
let check_pervasives =
  let loaded = ref false in
    fun () ->
      if !pervasives && not !loaded then begin
        Lang.load_libs ~parse_only:!parse_only () ;
        loaded := true
      end

let plugin_doc name =
  secondary_task := true ;
  check_pervasives () ;
  let found = ref false in
    List.iter
      (fun (lbl, i) ->
         match
           try Some (i#get_subsection name) with Not_found -> None
         with
           | None -> ()
           | Some s ->
               found := true ;
               Printf.printf "*** One entry in %s:\n" lbl ;
               let print =
                 if lbl="scripting values" then
                   Doc.print_lang
                 else
                   Doc.print
               in
                 print s)
      Plug.plugs#get_subsections ;
    if not !found then
      Printf.printf "Plugin not found!\n%!"

let eval src =
  check_pervasives () ;
  match src with
    | `StdIn ->
        Log.conf_stdout#set_d (Some true) ;
        Log.conf_file#set_d (Some false) ;
        Lang.from_in_channel ~parse_only:!parse_only stdin
    | `Expr_or_File expr when (not (Sys.file_exists expr)) ->
        Log.conf_stdout#set_d (Some true) ;
        Log.conf_file#set_d (Some false) ;
        Lang.from_string ~parse_only:!parse_only expr
    | `Expr_or_File f ->
        let basename = Filename.basename f in
        let basename =
          try Filename.chop_extension basename with _ -> basename
        in
          Configure.var_script := basename ;
          Lang.from_file ~parse_only:!parse_only f

let process_request s =
  secondary_task := true ;
  let req = Utils.get_some (Request.create s) in
    match Request.resolve req 20. with
      | Request.Failed ->
          Printf.printf "Request resolution failed.\n" ;
          Request.destroy req ;
          exit 2
      | Request.Timeout ->
          Printf.printf "Request resolution timeout.\n" ;
          Request.destroy req ;
          exit 1
      | Request.Resolved ->
          let metadata = Request.get_all_metadata req in
          let metadata = Request.string_of_metadata metadata in
            Printf.printf "Request resolved.\n%s\n" metadata ;
            Printf.printf "Computing duration: %!" ;
            begin try
              Printf.printf "%.2f sec.\n"
                (Request.duration (Utils.get_some (Request.get_filename req)))
            with
              Not_found -> Printf.printf "failed.\n"
            end ;
            Request.destroy req

module LiqConf =
struct
  (** Contains clones of Dtools.Conf.(descr|dump) but with a liq syntax. *)

  let format_string = Printf.sprintf "%S"
  let format_list l =
    "[" ^ (String.concat "," (List.map format_string l)) ^ "]"

  let get_string t =
    try
      match t#kind with
        | None -> None
        | Some "unit" -> Some "()"
        | Some "int" -> Some (string_of_int (Conf.as_int t)#get)
        | Some "float" -> Some (string_of_float (Conf.as_float t)#get)
        | Some "bool" -> Some (string_of_bool (Conf.as_bool t)#get)
        | Some "string" -> Some (format_string (Conf.as_string t)#get)
        | Some "list" -> Some (format_list (Conf.as_list t)#get)
        | _ -> assert false
    with
      | Conf.Undefined _ -> None

  let get_d_string t =
    let mapopt f = (function None -> None | Some x -> Some (f x)) in
      try
        match t#kind with
          | None -> None
          | Some "unit" -> mapopt (fun () -> "()") (Conf.as_unit t)#get_d
          | Some "int" -> mapopt string_of_int (Conf.as_int t)#get_d
          | Some "float" -> mapopt string_of_float (Conf.as_float t)#get_d
          | Some "bool" -> mapopt string_of_bool (Conf.as_bool t)#get_d
          | Some "string" -> mapopt format_string (Conf.as_string t)#get_d
          | Some "list" -> mapopt format_list (Conf.as_list t)#get_d
          | _ -> assert false
      with
        | Conf.Undefined _ -> None

  let string_of_path p =
    String.concat "." p

  let dump ?(prefix=[]) t =
    let rec aux prefix t =
      let p s = if prefix = "" then s else prefix ^ "." ^ s in
      let subs =
        List.map (function s -> aux (p s) (t#path [s])) t#subs
      in
        begin match get_d_string t, get_string t with
          | None, None ->
              "" (* Printf.sprintf "# set %-30s\n" prefix *)
          | Some p, None ->
              Printf.sprintf "# set(%S,%s)\n" prefix p
          | Some p, Some p' when p' = p ->
              Printf.sprintf "# set(%S,%s)\n" prefix p
          | _, Some p ->
              Printf.sprintf "set(%S,%s)\n" prefix p
        end ^
        String.concat "" subs
    in
      aux (string_of_path prefix) (t#path prefix)

  let descr ?(prefix=[]) t =
    let rec aux prefix t =
      let p s = if prefix = "" then s else prefix ^ "." ^ s in
      let subs =
        List.map (function s -> aux (p s) (t#path [s])) t#subs
      in
        begin match t#kind, get_string t with
        | None, None -> Printf.sprintf "## %s\n\n" t#descr
        | Some _, Some p ->
            Printf.sprintf "## %s\n"
            t#descr ^
            begin match get_d_string t with
              | None -> ""
              | Some d -> Printf.sprintf "# Default: %s\n" d
            end ^
            Printf.sprintf "set(%S,%s)\n" prefix p ^
            begin match t#comments with
              | [] -> ""
              | l ->
                  "# Comments:\n" ^
                  String.concat ""
                    (List.map (fun s -> Printf.sprintf "#  %s\n" s) l)
            end ^
            "\n"
        | _ -> ""
        end ^
        String.concat "" subs
    in
      aux (string_of_path prefix) (t#path prefix)

  let descr_key t p =
    try
      print_string (descr ~prefix:(Conf.path_of_string p) t);
      exit 0
    with
      | Dtools.Conf.Unbound _ ->
          Printf.eprintf "The key '%s' is not a valid configuration key.\n%!" p;
          exit 1

  let args t =
    [
      ["--conf-descr-key"],
      Arg.String (descr_key t),
      "Describe a configuration key.";
      ["--conf-descr"],
      Arg.Unit (fun () ->
        print_string (descr t); exit 0),
      "Display a described table of the configuration keys.";
      ["--conf-dump"],
      Arg.Unit (fun () ->
        print_string (dump t); exit 0),
      "Dump the configuration state";
    ]

end

let format_doc s =
  let prefix = "\t  " in
  let indent = 8+2 in
  let max_width = 80 in
  let s = Pcre.split ~pat:" " s in
  let s =
    let rec join line width = function
      | [] -> [line]
      | hd::tl ->
          let hdw = String.length hd in
          let w = width + 1 + hdw in
            if w < max_width then
              join (line^" "^hd) w tl
            else
              line :: join (prefix^hd) hdw tl
    in
      match s with
        | hd::tl ->
            join (prefix ^ hd) (indent + String.length hd) tl
        | [] -> []
  in
    String.concat "\n" s

let options =
  List.fold_left
    ( fun l (la,b,c) ->
        let ta = List.hd (List.rev la) in
        let expand = List.map
                       (fun a -> (a,b,(if a = ta then
                                         "\n" ^ format_doc c
                                       else
                                         "")))
                       la in
          l@expand ) []
    (let opts = [
      ["-"],
      Arg.Unit (fun () -> eval `StdIn),
      "Read script from standard input." ;

      ["-r"],
      Arg.String process_request,
      "Process a request." ;

      ["-h"],
      Arg.String plugin_doc,
      "Print the description of a plugin.";

      ["-c";"--check"],
      Arg.Unit (fun () ->
                  secondary_task := true ;
                  dont_run := true),
      "Check and evaluate scripts but do not perform any streaming." ;

      ["-p";"--parse-only"],
      Arg.Unit (fun () ->
                  secondary_task := true ;
                  parse_only := true),
      "Parse scripts but do not type-check and run them." ;

      ["-q";"--quiet"],
      Arg.Unit (fun () -> Log.conf_stdout#set false),
      "Do not print log messages on standard output." ;

      ["-v";"--verbose"],
      Arg.Unit (fun () -> Log.conf_stdout#set true),
      "Print log messages on standard output." ;

      ["--debug"],
      Arg.Unit (fun () -> Log.conf_level#set (max 4 Log.conf_level#get)),
      "Print debugging log messages." ;

      ["-d";"--daemon"],
      Arg.Unit (fun f -> Init.conf_daemon#set true),
      "Run in daemon mode." ;

      ["-t";"--enable-telnet"],
      Arg.Unit (fun _ -> Server.conf_telnet#set true),
      "Enable the telnet server." ;

      ["-T";"--disable-telnet"],
      Arg.Unit (fun _ -> Server.conf_telnet#set false),
      "Disable the telnet server." ;

      ["-u";"--enable-unix-socket"],
      Arg.Unit (fun _ -> Server.conf_socket#set true),
      "Enable the unix socket." ;

      ["-U";"--disable-unix-socket"],
      Arg.Unit (fun _ -> Server.conf_socket#set false),
      "Disable the unix socket." ;

      ["--list-plugins-xml"],
      Arg.Unit (fun () ->
                  secondary_task := true ;
                  check_pervasives () ;
                  Doc.print_xml (Plug.plugs:Doc.item)),
      Printf.sprintf
        "List all plugins (builtin scripting values, \
         supported formats and protocols), \
         output as XML." ;

      ["--list-plugins"],
      Arg.Unit (fun () ->
                  secondary_task := true ;
                  check_pervasives () ;
                  Doc.print (Plug.plugs:Doc.item)),
      Printf.sprintf
        "List all plugins (builtin scripting values, \
         supported formats and protocols)." ;

      ["--no-pervasives"],
      Arg.Clear pervasives,
      Printf.sprintf
        "Do not load pervasives script libraries (i.e., %s/*.liq)."
        Configure.libs_dir ;

      ["-i"],
      Arg.Set Configure.display_types,
      "Display infered types." ;

      ["--version"],
      Arg.Unit (fun () ->
                  Printf.printf
                    "Liquidsoap %s%s.\n\
                     Copyright (c) 2003-2008 Savonet team\n\
                     Liquidsoap is open-source software, \
                     released under GNU General Public License.\n\
                     See <http://savonet.sf.net> for more information.\n"
                    Configure.version SVN.rev ;
                  exit 0),
     "Display liquidsoap's version." ;

    ["--"],
    Arg.Unit (fun () -> Arg.current := Array.length Shebang.argv - 1),
    "Stop parsing the command-line and pass subsequent items to the script." ]

     in opts@LiqConf.args Configure.conf@Configure.dynliq_option)

let log = Log.make ["main"]

let () =
  log#f 3 "Liquidsoap %s%s" Configure.version SVN.rev

(** Just like Arg.parse_argv but with Arg.parse's behavior on errors.. *)
let parse argv l f msg =
  try
    Arg.parse_argv argv l f msg ;
  with
    | Arg.Bad msg -> Printf.eprintf "%s" msg ; exit 2
    | Arg.Help msg -> Printf.printf "%s" msg ; exit 0

let absolute s =
  if String.length s > 0 && s.[0] <> '/' then
    (Unix.getcwd ())^"/"^s
  else
    s

(* Startup *)
let () =

  Random.self_init () ;

  (* Set the default values. *)
  Log.conf_file_path#set_d (Some "<syslogdir>/<script>.log") ;
  Init.conf_daemon_pidfile#set_d (Some true) ;
  Init.conf_daemon_pidfile_path#set_d (Some "<sysrundir>/<script>.pid") ;

  (* Parse command-line, and notably load scripts. *)
  parse Shebang.argv options (fun s -> eval (`Expr_or_File s)) usage ;

  (* Now that the paths have their definitive value, expand <shortcuts>. *)
  let subst conf = conf#set (Configure.subst_vars conf#get) in
    subst Log.conf_file_path ;
    subst Init.conf_daemon_pidfile_path

(* Check that directories used by Dtools exist. *)
let check_directories () =
  let check_dir conf_path kind =
    let path = conf_path#get in
    let dir = Filename.dirname path in
      if not (Sys.file_exists dir) then begin
        Printf.printf "FATAL ERROR: %s directory %S does not exist.\n"
          kind dir ;
        let routes = Configure.conf#routes conf_path#ut in
          Printf.printf
            "To change it, add the following to your script:\n";
          Printf.printf "  set(%S, \"<path>\")\n"
            (Conf.string_of_path (List.hd routes)) ;
          exit 1
      end
  in
    if Log.conf_file#get then
      check_dir Log.conf_file_path "Log" ;
    if Init.conf_daemon#get && Init.conf_daemon_pidfile#get then
      check_dir Init.conf_daemon_pidfile_path "Pid"

(* Now that outputs have been defined, we can start the main loop. *)
let () =
  let root = ref (Thread.self ()) in
    let cleanup () =
      log#f 3 "Shutdown started!" ;
      Root.shutdown := true ;
      (* It's the root's job to ask the scheduler to shutdown,
       * but if the root died, we must do it. *)
      if not (Tutils.running "root" !root) then
        Root.force_sleep () ;
      Thread.delay 3. ;
      log#f 3 "Cleaning downloaded files..." ;
      Request.clean ()
    in
    let main () =
      root := Tutils.create Root.start () "root" ;
      Tutils.main ()
    in
      ignore (Init.at_stop cleanup) ;
      ignore (Init.at_stop (fun () -> Duppy.stop Tutils.scheduler)) ;
      if Source.has_outputs () then
        if not !dont_run then begin
          check_directories () ;
          Init.init ~prohibit_root:true main
        end else
          cleanup ()
      else
        (* If there's no output and no secondary task has been performed,
         * warn the user that his scripts didn't define any output. *)
        if not !secondary_task then
          Printf.printf "No output defined, nothing to do.\n"
