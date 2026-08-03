(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let log = Log.make ["lifecycle"]
let on_load_callbacks = ref []

let on_load ~name fn =
  let fn () =
    log#debug "Executing action %s (stage: load)" name;
    fn ()
  in
  on_load_callbacks := fn :: !on_load_callbacks

let loaded = ref false

let load () =
  if not !loaded then (
    List.iter (fun fn -> fn ()) !on_load_callbacks;
    loaded := true)

let action_atom stage =
  let is_done = Atomic.make false in
  let actions = Atomic.make [(fun _ -> log#debug "At stage: %S" stage)] in
  let on_action ~name fn =
    let fn is_done =
      log#debug "Executing action %s (stage: %s%s)" name stage
        (if is_done then ", done" else "");
      fn ()
    in
    if Atomic.get is_done then fn true
    else Atomic.set actions (fn :: Atomic.get actions)
  in
  let action () =
    if not (Atomic.exchange is_done true) then
      List.iter (fun fn -> fn false) (Atomic.get actions)
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
  after_init ~name:"script large" script_parse;
  after_script_parse ~name:"start" start;
  after_start ~name:"main loop" main_loop;
  after_main_loop ~name:"core shutdown" core_shutdown;
  after_core_shutdown ~name:"scheduler shutdown" scheduler_shutdown;
  after_scheduler_shutdown ~name:"final cleanup" final_cleanup
