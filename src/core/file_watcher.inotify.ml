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
  string list ->
  (unit -> unit) ->
  unwatch

let fd = ref (None : Unix.file_descr option)
let handlers = ref []
let m = Mutex.create ()
let log = Log.make ["inotify"]
let include_subdirs = false

let rec watchdog () =
  let fd = Option.get !fd in
  let handler =
    Mutex_utils.mutexify m (fun _ ->
        let events = Inotify.read fd in
        List.iter
          (fun (wd, _, _, _) ->
            match List.assoc wd !handlers with
              | callback -> callback ()
              | exception Not_found -> (
                  try Inotify.rm_watch fd wd with _ -> ()))
          events;
        [watchdog ()])
  in
  { Duppy.Task.priority = `Maybe_blocking; events = [`Read fd]; handler }

let watch : watch =
 fun ~pos e files callback ->
  List.iter
    (fun file ->
      if not (Sys.file_exists file) then
        Lang.raise_error ~pos
          ~message:
            (Printf.sprintf "File %s not found!"
               (Lang_string.quote_utf8_string file))
          "not_found")
    files;
  Mutex_utils.mutexify m
    (fun () ->
      if !fd = None then (
        fd := Some (Inotify.create ());
        Duppy.Task.add Tutils.scheduler (watchdog ()));
      let fd = Option.get !fd in
      let watchers =
        List.map
          (fun file ->
            let event_conv = function
              | `Modify ->
                  [
                    Inotify.S_Moved_to;
                    Inotify.S_Moved_from;
                    Inotify.S_Delete;
                    Inotify.S_Create;
                  ]
                  @ if Sys.is_directory file then [] else [Inotify.S_Modify]
            in
            let e = List.flatten (List.map event_conv e) in
            let watcher = Inotify.add_watch fd file e in
            (watcher, callback))
          files
      in
      handlers := watchers @ !handlers;
      Mutex_utils.mutexify m (fun () ->
          (try
             List.iter
               (fun (watcher, _) -> Inotify.rm_watch fd watcher)
               watchers
           with exn ->
             let bt = Printexc.get_backtrace () in
             Utils.log_exception ~log ~bt
               (Printf.sprintf "Error while removing file watch handler: %s"
                  (Printexc.to_string exn)));
          handlers :=
            List.filter (fun (_, file) -> not (List.mem file files)) !handlers))
    ()
