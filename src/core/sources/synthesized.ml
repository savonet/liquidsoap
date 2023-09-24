(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2023 Savonet team

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
class virtual source ?name ~seek duration =
  let track_size = Option.map Frame.main_of_seconds duration in
  object (self)
    inherit Source.source ?name ()
    method stype = if track_size = None then `Infallible else `Fallible
    val mutable remaining = track_size
    method private _is_ready ?frame:_ _ = remaining <> Some 0
    method seek x = if seek then x else 0
    method seek_source = (self :> Source.source)
    method self_sync = (`Static, false)

    method remaining =
      match remaining with None -> -1 | Some remaining -> remaining

    val mutable must_fail = false
    method abort_track = must_fail <- true
    method virtual private synthesize : Frame.t -> int -> int -> unit

    method private get_frame frame =
      if must_fail then (
        Frame.add_break frame (Frame.position frame);
        must_fail <- false;
        if track_size <> None then remaining <- Some 0)
      else (
        let off = Frame.position frame in
        let len =
          match remaining with
            | None -> Lazy.force Frame.size - off
            | Some r ->
                let len = min (Lazy.force Frame.size - off) r in
                remaining <- Some (r - len);
                len
        in
        self#synthesize frame off len;
        Frame.add_break frame (off + len))
  end
