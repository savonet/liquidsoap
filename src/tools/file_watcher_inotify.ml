(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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
        (fun (wd,_,_,file) ->
          let f = List.assoc wd !handlers in
          f ()
        ) events;
      [ watchdog () ])
  in
  { Duppy.Task.
    priority = Tutils.Maybe_blocking;
    events = [ `Read fd ];
    handler;
  }

let watch : File_watcher.watch = fun e file f ->
  Tutils.mutexify m (fun () ->
    if !fd = None then
      begin
        fd := Some (Inotify.create ());
        Duppy.Task.add Tutils.scheduler (watchdog ())
      end;
    let fd = Utils.get_some !fd in
    let event_conv = function
      | `Modify -> Inotify.S_Modify
    in
    let wd = Inotify.add_watch fd file (List.map event_conv e) in
    handlers := (wd,f) :: !handlers;
    let unwatch =
      Tutils.mutexify m (fun () ->
        Inotify.rm_watch fd wd;
        handlers := List.remove_assoc wd !handlers)
    in
    unwatch) ()

let () =
  Configure.file_watcher := watch
