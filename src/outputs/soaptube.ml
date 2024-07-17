
open Dtools

(*<section soaptube>*)

let log = Log.log ~label:"soaptube"

let usage =
  "\n" ^
  "Copyright (c) 2003 Savonet Team.\n" ^
  "This software is under GNU GPL v2.\n" ^
  "Read COPYING for more information.\n" ^
  "\n" ^
  "Liquidrelay relays liquidsoap output.\n" ^
  "Liquidrelay is part of the Savonet project : <http://savonet.sf.net>.\n\n" ^
  "Usage : liquidsoap [options]\n" ^
  "        where options are ...\n"

let input = ref ""

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
      Arg.String (fun f -> input := f),
      "Configuration file" ;

    ]

let anon_fun = fun s -> Printf.fprintf stderr "Unused argument '%s'\n" s

let ignore_sigpipe =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let read_conf f =
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

    try Dtools.Conf.read_file f with _ ->
      failwith (Printf.sprintf "Cannot read conf file %S !" f)

let log = Log.log ~label:"output"

(* Where to get the RTP stream *)
let ip = Dtools.Conf.get_string ~default:"224.0.1.20" "rtp.address"
let port = Dtools.Conf.get_int ~default:8888 "rtp.port"
let new_session () = Rtp.new_session Rtp.Recv ip port
let session = ref (new_session ())
let wav = Mixer.Buffer.create ()

let loop f =
  let cr = Mutex.create () in
  let cw = Mutex.create () in

  let encoder () =
    let cp =
      let b = Mixer.Buffer.create () in
        Mixer.Buffer.set_already b Mixer.Buffer.size ;
        b
    in
      while true do
        Mutex.lock cr ;

        String.blit
          (Mixer.Buffer.to_string wav) 0 (Mixer.Buffer.to_string cp) 0
          Mixer.Buffer.size ;
        Mixer.Buffer.free_metadatas cp ;
        begin match Mixer.Buffer.get_metadata wav with
          | None -> ()
          | Some m -> Mixer.Buffer.push_metadata cp m
        end ;

        Mutex.unlock cw ;
        f cp
      done
  in
    Mutex.lock cr ;
    ignore (Thread.create encoder ()) ;
    while true do
      Mutex.lock cw ;

      if Rtp.recv !session wav then
        ( log 2 "Garbage on the session: rebooting it!" ;
          session := new_session () ) ;

      Mutex.unlock cr
    done

let _ =
  Arg.parse options anon_fun usage ;
  read_conf !input ;
  let output = Output.from_conf () in
    Init.init
      (fun () ->
         while (Mixer.Buffer.to_string wav).[0] = '\000' do
           if Rtp.recv !session wav then
             (Mixer.Buffer.to_string wav).[0] <- '\000'
         done ;
         log 2 "Stream started!" ;
         output loop )
