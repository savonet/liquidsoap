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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let log = Log.make ["lifecycle"]

let make_action ?(before = []) ?(after = []) name =
  let is_done = ref false in
  let on_actions =
    ref
      [
        (fun () ->
          is_done := true;
          log#debug "At stage: %S" name);
      ]
  in
  let on_action () = List.iter (fun fn -> fn ()) !on_actions in
  let atom = Dtools.Init.make ~before ~after ~name on_action in
  let before_action f =
    if !is_done then f () else ignore (Dtools.Init.make ~before:[atom] f)
  in
  let on_action fn =
    if !is_done then fn () else on_actions := !on_actions @ [fn]
  in
  let after_action f =
    if !is_done then f () else ignore (Dtools.Init.make ~after:[atom] f)
  in
  (atom, before_action, on_action, after_action)

(* This atom is explicitly triggered in [Main] *)
let init_atom, before_init, on_init, after_init =
  make_action "Liquidsoap initialization"

let script_parse_atom, before_script_parse, on_script_parse, after_script_parse
    =
  make_action ~after:[init_atom] "Liquidsoap script parse"

let start_atom, before_start, on_start, after_start =
  make_action ~after:[script_parse_atom] "Liquidsoap application start"

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

let main_loop_ref = ref (fun () -> assert false)
let main_loop fn = main_loop_ref := fn

let _ =
  Dtools.Init.make ~after:[start_atom] (fun () ->
      let fn = !main_loop_ref in
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
        exit (-1)
      in
      try Dtools.Init.init ~prohibit_root:(not allow_root#get) fn
      with Dtools.Init.Root_prohibited e -> on_error e)

let ( final_cleanup_atom,
      before_final_cleanup,
      on_final_cleanup,
      after_final_cleanup ) =
  make_action ~before:[Dtools.Init.stop] "Liquidsoap final cleanup"

let ( scheduler_shutdown_atom,
      before_scheduler_shutdown,
      on_scheduler_shutdown,
      after_scheduler_shutdown ) =
  make_action ~before:[final_cleanup_atom] "Liquidsoap scheduler shutdown"

let _, before_core_shutdown, on_core_shutdown, after_core_shutdown =
  make_action ~before:[scheduler_shutdown_atom] "Liquidsoap core shutdown"

let _, before_stop, on_stop, after_stop =
  make_action ~after:[Dtools.Log.stop] "Liquidsoap application ended"
