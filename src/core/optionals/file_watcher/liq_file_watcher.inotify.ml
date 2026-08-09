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

(* An inotify watch follows the inode, not the path. Watching a file directly
   therefore stops reporting as soon as that file is replaced rather than
   modified in place, which is what a writer doing the usual write-to-temporary
   then rename does. We watch the containing directory instead and keep the
   basename to sort out which events are ours. *)
type handler = {
  wd : Inotify.watch;
  basename : string option;
  callback : unit -> unit;
}

let fd = ref (None : Unix.file_descr option)
let handlers = ref ([] : handler list)
let m = Mutex.create ()
let log = Log.make ["inotify"]

let rec watchdog () =
  let fd = Option.get !fd in
  let handler =
    Mutex_utils.mutexify m (fun _ ->
        let events = Inotify.read fd in
        List.iter
          (fun (wd, _, _, name) ->
            (* Watch descriptors are per path, so watches on two files of the
               same directory share one and all of them have to be considered. *)
              match List.filter (fun h -> h.wd = wd) !handlers with
              | [] -> ( try Inotify.rm_watch fd wd with _ -> ())
              | l ->
                  List.iter
                    (fun h ->
                      if h.basename = None || h.basename = name then
                        h.callback ())
                    l)
          events;
        [watchdog ()])
  in
  { Duppy.Task.priority = `Maybe_blocking; events = [`Read fd]; handler }

let watch : watch =
 fun ~pos e file f ->
  if not (Sys.file_exists file) then Lang.raise_error ~pos "not_found";
  Mutex_utils.mutexify m
    (fun () ->
      if !fd = None then (
        fd := Some (Inotify.create ());
        Duppy.Task.add Tutils.scheduler (watchdog ()));
      let fd = Option.get !fd in
      let watched, basename =
        if Sys.is_directory file then (file, None)
        else (Filename.dirname file, Some (Filename.basename file))
      in
      let event_conv = function
        | `Modify ->
            [
              Inotify.S_Moved_to;
              Inotify.S_Moved_from;
              Inotify.S_Delete;
              Inotify.S_Create;
            ]
            @ if basename = None then [] else [Inotify.S_Modify]
      in
      let e = List.flatten (List.map event_conv e) in
      let wd = Inotify.add_watch fd watched e in
      let handler = { wd; basename; callback = f } in
      handlers := handler :: !handlers;
      Mutex_utils.mutexify m (fun () ->
          handlers := List.filter (fun h -> h != handler) !handlers;
          if not (List.exists (fun h -> h.wd = wd) !handlers) then (
            try Inotify.rm_watch fd wd
            with exn ->
              let bt = Printexc.get_backtrace () in
              Utils.log_exception ~log ~bt
                (Printf.sprintf "Error while removing file watch handler: %s"
                   (Printexc.to_string exn)))))
    ()
