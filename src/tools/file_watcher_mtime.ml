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

let launched = ref false

let watched = ref []

let m = Mutex.create ()

let file_mtime file = (Unix.stat file).Unix.st_mtime

let rec watchdog () =
  let handler =
    Tutils.mutexify m (fun _ ->
        watched :=
          List.map
            (fun (file, mtime, f) ->
              let mtime' = try file_mtime file with _ -> mtime in
              if mtime' <> mtime then f () ;
              (file, mtime', f))
            !watched ;
        [watchdog ()])
  in
  {Duppy.Task.priority= Tutils.Maybe_blocking; events= [`Delay 1.]; handler}

let watch : File_watcher.watch =
 fun e file f ->
  if List.mem `Modify e then
    Tutils.mutexify m
      (fun () ->
        if not !launched then (
          launched := true ;
          Duppy.Task.add Tutils.scheduler (watchdog ()) ) ;
        let mtime = try file_mtime file with _ -> 0. in
        watched := (file, mtime, f) :: !watched ;
        let unwatch =
          Tutils.mutexify m (fun () ->
              watched :=
                List.filter (fun (fname, _, _) -> fname <> file) !watched)
        in
        unwatch)
      ()
  else fun () -> ()
