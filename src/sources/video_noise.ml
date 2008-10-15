(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

(** Generate a saw *)

open Source

  (*
let random_string n =
  let s = String.create n in
    for i = 0 to n - 1 do
      s.[i] <- char_of_int (Random.int 256)
    done;
    s
   *)

let noise_frame () =
  let frame = RGB.create (Fmt.video_width ()) (Fmt.video_height ()) in
    (*
    for i = 0 to Fmt.video_width () - 1 do
      for j = 0 to Fmt.video_height () - 1 do
        RGB.set frame i j (Random.int 256, Random.int 256, Random.int 256)
      done
    done;
     *)
    RGB.randomize frame;
    frame

class noise duration =
  let nb_frames = Fmt.video_frames_of_seconds duration in
object
  inherit source

  method stype = Infallible
  method is_ready = true

  val mutable remaining = nb_frames
  method remaining = Fmt.ticks_of_video_frames remaining

  val mutable must_fail = false
  method abort_track =
    must_fail <- true;
    remaining <- 0

  method get_frame ab =
    if must_fail then begin
      VFrame.add_break ab (VFrame.position ab);
      remaining <- nb_frames;
      must_fail <- false
    end else
      let b = VFrame.get_rgb ab in
      let off = VFrame.position ab in
      let size = VFrame.size ab in
        for c = 0 to Array.length b - 1 do
          let buf_c = b.(c) in
            for i = off to size - 1 do
              buf_c.(i) <- noise_frame ()
            done;
        done;
        AFrame.add_break ab (AFrame.size ab) ;
        remaining <- remaining - (AFrame.size ab) - off ;
        if remaining <= 0 then must_fail <- true

end

let () =
  Lang.add_operator "video.noise"
    ~category:Lang.Input
    ~descr:"Generate white noise."
    [
      "duration", Lang.float_t, Some (Lang.float 0.), None
    ]
    (fun p ->
       (new noise
          (Lang.to_float (List.assoc "duration" p)) :> source))
