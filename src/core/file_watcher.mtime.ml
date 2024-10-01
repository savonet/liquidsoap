(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Event to watch. *)
type event = [ `Modify ]

(** Type for unwatching function. *)
type unwatch = unit -> unit

(** Type for watching function. *)
type watch =
  pos:Liquidsoap_lang.Pos.t list ->
  event list ->
  string ->
  (unit -> unit) ->
  unwatch

type watched_files = {
  file : string;
  callback : unit -> unit;
  mutable mtime : float;
}

let log = Log.make ["file_watcher"; "native"]
let launched = ref false
let watched = ref []
let m = Mutex.create ()
let file_mtime file = (Unix.stat file).Unix.st_mtime

let rec handler _ =
  Mutex_utils.mutexify m
    (fun () ->
      List.iter
        (fun ({ file; callback; mtime } as w) ->
          try
            let mtime' = try file_mtime file with _ -> mtime in
            if mtime' <> mtime then callback ();
            w.mtime <- mtime'
          with exn ->
            let bt = Printexc.get_backtrace () in
            Utils.log_exception ~log ~bt
              (Printf.sprintf "Error while executing file watcher callback: %s"
                 (Printexc.to_string exn)))
        !watched;
      [{ Duppy.Task.priority = `Generic; events = [`Delay 1.]; handler }])
    ()

let watch : watch =
 fun ~pos e file callback ->
  if not (Sys.file_exists file) then Lang.raise_error ~pos "not_found";
  if List.mem `Modify e then
    Mutex_utils.mutexify m
      (fun () ->
        if not !launched then begin
          launched := true;
          Duppy.Task.add Tutils.scheduler
            { Duppy.Task.priority = `Generic; events = [`Delay 1.]; handler }
        end;
        let mtime = try file_mtime file with _ -> 0. in
        watched := { file; mtime; callback } :: !watched;
        let unwatch =
          Mutex_utils.mutexify m (fun () ->
              watched := List.filter (fun w -> w.file <> file) !watched)
        in
        unwatch)
      ()
  else fun () -> ()
