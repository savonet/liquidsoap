(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Source
open Genlex
exception Error

class insert_metadata ~kind source =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable metadata = None
  val lock_m = Mutex.create ()
  val mutable ns = []

  method insert_metadata m = 
    Mutex.lock lock_m ;
    metadata <- Some m ;
    Mutex.unlock lock_m

  method private get_frame buf =
    let p = Frame.position buf in
      source#get buf ;
      Mutex.lock lock_m ;
      Tutils.finalize ~k:(fun () -> Mutex.unlock lock_m)
      (fun () -> 
        match metadata with
          | Some m ->
              Frame.set_metadata buf p m ;
              metadata <- None
          | None -> ())

end

(** Insert metadata at the beginning if none is set.
  * Currently used by the switch classes. *)
class replay ~kind meta src =
object (self)
  inherit operator kind [src]

  val mutable first = true

  method stype = src#stype
  method is_ready = src#is_ready
  method abort_track = src#abort_track
  method remaining = src#remaining

  method private get_frame ab =
    let start = Frame.position ab in
      src#get ab ;
      if first then begin
        if Frame.get_metadata ab start = None then
          Frame.set_metadata ab start meta ;
        first <- false
      end
end
