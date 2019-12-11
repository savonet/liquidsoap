(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type event = [`Modify]

let fd = ref (None : Unix.file_descr option)

let handlers = ref []

let m = Mutex.create ()

let rec watchdog () =
  let fd = Utils.get_some !fd in
  let handler =
    Tutils.mutexify m (fun _ ->
        let events = Inotify.read fd in
        List.iter
          (fun (wd, _, _, _) ->
            let f = List.assoc wd !handlers in
            f ())
          events ;
        [watchdog ()])
  in
  {Duppy.Task.priority= Tutils.Maybe_blocking; events= [`Read fd]; handler}

let watch : File_watcher.watch =
 fun e file f ->
  Tutils.mutexify m
    (fun () ->
      if !fd = None then (
        fd := Some (Inotify.create ()) ;
        Duppy.Task.add Tutils.scheduler (watchdog ()) ) ;
      let fd = Utils.get_some !fd in
      let event_conv = function
        | `Modify ->
            [ Inotify.S_Modify;
              Inotify.S_Moved_to;
              Inotify.S_Moved_from;
              Inotify.S_Delete;
              Inotify.S_Create ]
      in
      let e = List.flatten (List.map event_conv e) in
      let wd = Inotify.add_watch fd file e in
      handlers := (wd, f) :: !handlers ;
      let unwatch =
        Tutils.mutexify m (fun () ->
            Inotify.rm_watch fd wd ;
            handlers := List.remove_assoc wd !handlers)
      in
      unwatch)
    ()

let () = Configure.file_watcher := watch
