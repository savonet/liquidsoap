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

open Source

(** Insert metadata at the beginning if none is set. Currently used by the
    switch classes. *)
class replay meta src =
  object
    inherit operator ~name:"replay_metadata" [src]
    val mutable first = true
    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method seek_source = src#seek_source

    method private generate_frame =
      let buf = src#get_frame in
      if first then (
        first <- false;
        if Frame.get_all_metadata buf = [] then Frame.add_metadata buf 0 meta
        else buf)
      else buf
  end
