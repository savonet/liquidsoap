(**************************************************************************)
(*  ocaml-dtools                                                          *)
(*  Copyright (C) 2003-2010  The Savonet Team                             *)
(**************************************************************************)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; either version 2 of the License, or     *)
(*  any later version.                                                    *)
(**************************************************************************)
(*  Contact: savonet-devl@lists.sourceforge.net                           *)
(**************************************************************************)

(* $Id$ *)

(* Syslog logging. *)

open Dtools_impl

let conf_syslog =
  Conf.bool ~p:(Log.conf#plug "syslog") ~d:false "Enable syslog logging."

let conf_program =
  Conf.string
    ~p:(conf_syslog#plug "program")
    ~d:(Filename.basename Sys.executable_name)
    "Name of the program."

let conf_facility =
  Conf.string ~p:(conf_syslog#plug "facility") ~d:"DAEMON" "Logging facility."

let logging = ref None

let () =
  let start () =
    if conf_syslog#get then (
      let facility = Syslog.facility_of_string conf_facility#get in
      let program = Printf.sprintf "%s[%d]" conf_program#get (Unix.getpid ()) in
      let log = Syslog.openlog ~facility program in
      logging := Some log;
      let exec s = Syslog.syslog log `LOG_INFO s in
      Log.add_custom_log program { Log.timestamp = false; exec })
  in
  let stop () = match !logging with Some x -> Syslog.closelog x | _ -> () in
  ignore (Init.at_start ~before:[Log.start] start);
  ignore (Init.at_stop ~after:[Log.stop] stop)
