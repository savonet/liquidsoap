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

(* For some synthesized function (e.g. noise, blank, sine), we can pretend we
   support seek by doing nothing. However, for other, seek should be
   disabled. Thus, if [seek] is [true], the seek function is [fun x -> x]
   otherwise it is [fun _ -> 0] *)
class virtual source ~name ~seek duration =
  let track_size = Option.map Frame.main_of_seconds duration in
  object (self)
    inherit Source.source ~name ()
    method fallible = track_size <> None
    val mutable remaining = track_size
    method private can_generate_frame = remaining <> Some 0

    method! seek x =
      match (seek, remaining) with
        | false, _ -> 0
        | true, None -> x
        | true, Some r when x <= r ->
            remaining <- Some (r - x);
            x
        | true, Some r ->
            remaining <- Some 0;
            r

    method seek_source = (self :> Source.source)
    method self_sync = (`Static, None)

    method remaining =
      match remaining with None -> -1 | Some remaining -> remaining

    val mutable add_track_mark = true

    method abort_track =
      add_track_mark <- true;
      remaining <- Some (-1)

    method virtual private synthesize : int -> Frame.t

    method private generate_frame =
      let len =
        match remaining with
          | Some -1 | None -> Lazy.force Frame.size
          | Some r ->
              let len = min (Lazy.force Frame.size) r in
              remaining <- Some (r - len);
              len
      in
      let buf = self#synthesize len in
      if add_track_mark then (
        add_track_mark <- false;
        remaining <- track_size;
        Frame.add_track_mark buf 0)
      else buf
  end
