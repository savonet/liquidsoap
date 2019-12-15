(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

(* For some synthetized function (e.g. noise, blank, sine),
 * we can pretend we support seek by doing nothing. However, for
 * other, seek should be disabled. Thus, if [seek] is [true],
 * the seek function is [fun x -> x] otherwise it is
 * [fun _ -> 0] *)
class virtual source ?name ~seek kind duration =
  let track_size =
    if duration <= 0. then None else Some (Frame.master_of_seconds duration)
  in
  object (self)
    inherit Source.source ?name kind

    method stype = Source.Infallible

    method is_ready = true

    method seek x = if seek then x else 0

    method self_sync = false

    val mutable remaining = track_size

    method remaining =
      match remaining with None -> -1 | Some remaining -> remaining

    val mutable must_fail = false

    method abort_track = must_fail <- true

    method virtual private synthesize : Frame.t -> int -> int -> unit

    method private get_frame frame =
      if must_fail then (
        Frame.add_break frame (Frame.position frame);
        remaining <- track_size;
        must_fail <- false )
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
        Frame.add_break frame (off + len);
        if VFrame.is_partial frame then (
          assert (remaining = Some 0);
          remaining <- track_size ) )
  end
