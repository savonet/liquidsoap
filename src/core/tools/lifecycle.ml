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
let on_load_callbacks = ref []
let on_load fn = on_load_callbacks := fn :: !on_load_callbacks
let loaded = ref false

let load () =
  if not !loaded then (
    List.iter (fun fn -> fn ()) !on_load_callbacks;
    loaded := true)

let action_atom name =
  let is_done = Atomic.make false in
  let actions = Atomic.make [(fun () -> log#debug "At stage: %S" name)] in
  let on_action fn =
    if Atomic.get is_done then fn ()
    else Atomic.set actions (fn :: Atomic.get actions)
  in
  let action () =
    if not (Atomic.exchange is_done true) then
      List.iter (fun fn -> fn ()) (Atomic.get actions)
  in
  (on_action, action)

let make_action name =
  let on_before, before_action = action_atom ("before " ^ name) in
  let on_action, action = action_atom name in
  let on_after, after_action = action_atom ("after " ^ name) in
  let action () =
    before_action ();
    action ();
    after_action ()
  in

  (action, on_before, on_action, on_after)

let init, before_init, on_init, after_init =
  make_action "Liquidsoap initialization"

let script_parse, before_script_parse, on_script_parse, after_script_parse =
  make_action "Liquidsoap script parse"

let start, before_start, on_start, after_start =
  make_action "Liquidsoap application start"

let main_loop, before_main_loop, on_main_loop, after_main_loop =
  make_action "Liquidsoap main loop"

let core_shutdown, before_core_shutdown, on_core_shutdown, after_core_shutdown =
  make_action "Liquidsoap core shutdown"

let ( scheduler_shutdown,
      before_scheduler_shutdown,
      on_scheduler_shutdown,
      after_scheduler_shutdown ) =
  make_action "Liquidsoap scheduler shutdown"

let final_cleanup, before_final_cleanup, on_final_cleanup, after_final_cleanup =
  make_action "Liquidsoap final cleanup"

let () =
  after_init script_parse;
  after_script_parse start;
  after_start main_loop;
  after_main_loop core_shutdown;
  after_core_shutdown scheduler_shutdown;
  after_scheduler_shutdown final_cleanup
