(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

let usage = "Usage : liquidsoap [option ...] [EXPR or SCRIPT.liq]\n"

let infile = ref None
let inline = ref false
let dont_run = ref false
let list = ref false
let list_xml = ref false
let stdout_set = ref false

let options =
  List.fold_left
    ( fun l (la,b,c) ->
	let ta = List.hd (List.rev la) in
	let expand = List.map
		       (fun a -> (a,b,(if a = ta then ("\n\t  "^c) else "")))
		       la in
	  l@expand ) []
    (let opts = [

      ["-c";"--check"],
      Arg.Set dont_run,
      "Only check the program." ;

      ["-q";"--quiet"],
      Arg.Unit (fun () -> stdout_set:=true ; Conf.set_bool "log.stdout" false),
      "Do not print log messages on standard output" ;

      ["-v";"--verbose"],
      Arg.Unit (fun () -> stdout_set:=true ; Conf.set_bool "log.stdout" true),
      "Print log messages on standard output" ;

      ["--debug"],
      Arg.Unit (fun () -> Conf.set_int "log.level" 4),
      "Print debugging log messages." ;

      ["-d";"--daemon"],
      Arg.Unit (fun f -> Conf.set_bool "daemon" true),
      "Run in daemon mode" ;

      ["-s";"--enable-server"],
      Arg.Unit (fun _ -> Conf.set_bool "server" true),
      "Enable the telnet server" ;

      ["-S";"--disable-server"],
      Arg.Unit (fun _ -> Conf.set_bool "server" false),
      "Disable the telnet server" ;

      ["--"],
      Arg.Unit (fun () -> infile := Some "--"),
      "Read script from standard input." ;
      
      ["--list-plugins-xml"],
      Arg.Set list_xml,
      Printf.sprintf "List all %s, output as XML."
        (String.concat ", " (Plug.list ())) ;

      ["--list-plugins"],
      Arg.Set list,
      Printf.sprintf "List all %s."
        (String.concat ", " (Plug.list ())) ]
     in opts@Configure.dynliq_option)

let anon_fun = fun s -> infile := Some s

(* When launched from a #! script, liquidsoap restarts itself,
   calling the shell for parsing its command line. *)

let absolute s =
  if String.length s > 0 && s.[0] <> '/' then
    (Unix.getcwd ())^"/"^s
  else
    s

let _ =
  Var.register "daemon.piddir" Var.String ;
  Var.register "log.dir" Var.String

(* Startup ! *)

let () =
  Arg.parse options anon_fun usage ;
  if !list_xml then
    ( Doc.print_xml (Plug.plugs:Doc.item) ; exit 0 ) ;
  if !list then
    ( Doc.print (Plug.plugs:Doc.item) ; exit 0 )

let () =

  Random.self_init () ;

  begin
    match !infile with
      | None ->
          Printf.printf
            "No script file to process, exiting. Use --help for help.\n" ;
          exit 0
      | Some "--" -> Lang_user.from_in_channel stdin
      | Some expr when not (Sys.file_exists expr) ->
          Printf.printf
            "Input isn't a file. Treating it as an expression.\n" ;
          Dtools.Conf.set_bool "server"
            (Dtools.Conf.get_bool ~default:false "server") ;
          if not !stdout_set then
            Dtools.Conf.set_bool "log.stdout" true ;
          Dtools.Conf.set_string "log.dir" "/dev" ;
          Dtools.Conf.set_string "log.file" "null" ;
          Lang_user.from_string expr
      | Some s when not (Str.string_match (Str.regexp ".*\\.liq$") s 0) ->
          Printf.printf
            "Input isn't a .liq script. Treating it as a request.\n" ;
          let req = Utils.get_some (Request.create s) in
            begin match Request.resolve req 20. with
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
                    Request.destroy req ;
                    exit 0
            end
      | Some f ->
	  let basename = Filename.basename f in
	  let basename =
	    try Filename.chop_extension basename with _ -> basename
	  in
	  let default_log = basename ^ ".log" in
	  let default_pid = basename ^ ".pid" in

	    Dtools.Conf.set_string "log.file"
              ( let i = Dtools.Conf.get_string "log.file" in
		  (* log.file is a builtin key, default is "" *)
		  if i = "" then default_log else i ) ;

	    Dtools.Conf.set_string "daemon.pidfile"
              ( let i = Dtools.Conf.get_string "daemon.pidfile" in
		  (* It is also a builtin key, default is "" *)
		  if i = "" then default_pid else i ) ;

	    Lang_user.from_file f
  end ;

    Dtools.Conf.set_string "log.dir"
      (Dtools.Conf.get_string ~default:Configure.logdir "log.dir") ;
    Dtools.Conf.set_string "log.file"
      ( (Dtools.Conf.get_string "log.dir")^"/"^
	(Dtools.Conf.get_string "log.file")) ;

    Dtools.Conf.set_string "daemon.piddir"
      (Dtools.Conf.get_string ~default:Configure.piddir "daemon.piddir"); 
    Dtools.Conf.set_string "daemon.pidfile"
      ( (Dtools.Conf.get_string "daemon.piddir")^"/"^
	(Dtools.Conf.get_string "daemon.pidfile")) ;

    let root = ref (Thread.self ()) in
    let cleanup () =
      Log.log ~label:"main" 3 "Shutdown started !" ;
      Root.shutdown := true ;
      (* It's the root's job to ask the scheduler to shutdown,
       * but if the root died, we must do it. *)
      if not (Tutils.running "root" !root) then
        Root.force_sleep () ;
      Thread.delay 3. ;
      Log.log ~label:"main" 3 "Cleaning downloaded files..." ;
      Request.clean ()
    in
    let main () =
      root := Tutils.create Root.start () "root" ;
      Tutils.main ()
    in
      ignore (Init.at_stop cleanup) ;
      if not !dont_run then
        Init.init ~prohibit_root:true main
      else
        cleanup ()
