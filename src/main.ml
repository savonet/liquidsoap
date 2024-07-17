
open Root
open Dtools
open Printf

let usage =
  "\n" ^
  "Copyright (c) 2003 Savonet Team.\n" ^
  "This software is under GNU GPL v2.\n" ^
  "Read COPYING for more information.\n" ^
  "\n" ^
  "Liquidsoap is a programmable audio stream generator.\n" ^
  "Liquidsoap is part of the Savonet project : <http://savonet.sf.net>.\n\n" ^
  "Usage : liquidsoap [options] [script.liq]\n"^
  "        where options are ...\n"

let infile = ref None
let dont_run = ref false
let safety_check = ref false
let liveness_check = ref false
let list = ref false
let list_xml = ref false

let options =
  List.fold_left
    ( fun l (la,b,c) ->
	let ta = List.hd (List.rev la) in
	let expand = List.map
		       (fun a -> (a,b,(if a = ta then ("\n\t  "^c) else "")))
		       la in
	  l@expand ) []
    [

      ["-d";"--daemon"],
      Arg.Unit (fun f -> Conf.set_bool "daemon" true),
      "Run in daemon mode" ;

      ["-l";"--load-conf-file"],
      Arg.String (fun f -> try Conf.read_file f with _ ->
		    failwith (sprintf "Couldn't read conf file %S !" f)),
      "Configuration file" ;

      ["-c";"--check"],
      Arg.Set dont_run,
      "Only check the program." ;

      ["-s";"--safety-check"],
      Arg.Set safety_check,
      "Check that scheduler is safe." ;

      ["-a";"--liveness-check"],
      Arg.Set liveness_check,
      "Check that scheduler is alive." ;

      ["--list-plugins"],
      Arg.Set list_xml,
      Printf.sprintf "List all %s."
	(String.concat ", " (Plug.list ())) ;

      ["--list-plugins-human"],
      Arg.Set list,
      Printf.sprintf "List all %s."
	(String.concat ", " (Plug.list ())) ;

    (* TODO Dtools.Options.list *)

    ]

let anon_fun = fun s -> infile := Some s

(* When launched from a #! script, liquidsoap restarts itself,
   calling the shell for parsing its command line. *)

let absolute s =
  if String.length s > 0 && s.[0] <> '/' then
    (Unix.getcwd ())^"/"^s
  else
    s

(* Startup ! *)

let () =
  Arg.parse options anon_fun usage ;
  if !list_xml then
    ( Doc.print_xml (Plug.plugs:Doc.item) ; exit 0 ) ;
  if !list then
    ( Doc.print (Plug.plugs:Doc.item) ; exit 0 )

let () =

  Random.self_init () ;

  let scheduler =
    match !infile with
      | None -> Lang_user.from_in_channel !safety_check !liveness_check stdin
      | Some f ->
	  let basename = Filename.basename f in
	  let basename =
	    try Filename.chop_extension basename with _ -> basename
	  in
	  let default_log = basename ^ ".log" in
	  let default_pid = basename ^ ".pid" in

	    Dtools.Conf.set_string "log.file"
	      (*<ignore>*)
              ( let i = Dtools.Conf.get_string "log.file" in
		  (* log.file is a builtin key, default is "" *)
		  if i = "" then default_log else i ) ;

	    Dtools.Conf.set_string "daemon.pidfile"
	      (*<ignore>*)
              ( let i = Dtools.Conf.get_string "daemon.pidfile" in
		  (* It is also a builtin key, default is "" *)
		  if i = "" then default_pid else i ) ;

	    Lang_user.from_file !safety_check !liveness_check f
  in

    Dtools.Conf.set_string "log.dir"
      (* The log file will be <log.dir>/<log.file>. *) 
      (Dtools.Conf.get_string ~default:Configure.logdir "log.dir") ;
    Dtools.Conf.set_string "log.file"
      ( (*<ignore>*)
	(Dtools.Conf.get_string "log.dir")^"/"^
	(* Default is set to <name>.log if radio program is <name>.liq *)
	(Dtools.Conf.get_string "log.file")) ;

    Dtools.Conf.set_string "daemon.piddir"
      (* The pid file will be <daemon.piddir>/<daemon.pidfile> *) 
      (Dtools.Conf.get_string ~default:Configure.piddir "daemon.piddir"); 
    Dtools.Conf.set_string "daemon.pidfile"
      ( (*<ignore>*)
	(Dtools.Conf.get_string "daemon.piddir")^"/"^
	(* Default is set to <name>.pid if radio program is <name>.liq *)
	(Dtools.Conf.get_string "daemon.pidfile")) ;

    let root = ref (Thread.self ()) in
    let cleanup () =
      Log.log ~label:"main" 3 "Shutdown started !" ;
      Root.shutdown := true ;
      (* It's the root's job to ask the scheduler to shutdown,
       but if the root died, we must do it. *)
      if not (Tutils.running "root" !root) then
        scheduler#sleep ;
      Tutils.join_all () ;
      Log.log ~label:"main" 3 "Cleaning downloaded files..." ;
      Request.clean ()
    in
    let main () =
      root := Tutils.create (fun () -> Root.start scheduler) () "root" ;
      Tutils.raise_everything ()
    in
      ignore (Init.at_stop cleanup) ;
      if not !dont_run then
        Init.init ~prohibit_root:true main
      else
        Init.init (fun () -> ())
