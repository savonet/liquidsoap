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

class virtual source ~name duration =
  let track_size = Option.map Frame.main_of_seconds duration in
  object (self)
    inherit Source.source ~name ()
    method fallible = track_size <> None
    val mutable remaining = track_size
    method private can_generate_frame = remaining <> Some 0
    method! exhausted = remaining = Some 0
    method! endless = track_size = None

    method! private frame_end frame =
      if remaining = Some 0 then `Track_end
      else if Frame.is_partial frame then `Late
      else `Full

    method! seek x =
      match remaining with
        | None -> x
        | Some r when x <= r ->
            remaining <- Some (r - x);
            x
        | Some r ->
            remaining <- Some 0;
            r

    method effective_source = (self :> Source.source)
    method private self_sync = (`Static, None)

    method remaining =
      match remaining with None -> -1 | Some remaining -> remaining

    val mutable add_track_mark = true

    method abort_track =
      add_track_mark <- true;
      remaining <- Some (-1)

    method virtual private synthesize : int -> Frame.t

    method private generate_frame =
      let starts_track = add_track_mark in
      if starts_track then (
        add_track_mark <- false;
        remaining <- track_size);
      let len =
        match remaining with
          | None -> Lazy.Mutexed.force Frame.size
          | Some r ->
              let len = min (Lazy.Mutexed.force Frame.size) r in
              remaining <- Some (r - len);
              len
      in
      let buf = self#synthesize len in
      if starts_track then Frame.add_track_mark buf 0 else buf
  end
