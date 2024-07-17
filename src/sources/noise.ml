(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class noise duration =
  let nb_samples = Fmt.samples_of_seconds duration in
object
  inherit source

  method stype = Infallible
  method is_ready = true

  val mutable remaining = nb_samples
  method remaining = Fmt.ticks_of_samples remaining

  val mutable must_fail = false
  method abort_track =
    must_fail <- true;
    remaining <- 0

  method get_frame ab =
    if must_fail then begin
      AFrame.add_break ab (AFrame.position ab);
      remaining <- nb_samples ;
      must_fail <- false
    end else
      let b = AFrame.get_float_pcm ab in
      let off = AFrame.position ab in
      let size = AFrame.size ab in
        for c = 0 to Array.length b - 1 do
          let buf_c = b.(c) in
            for i = off to size - 1 do
              buf_c.(i) <- Random.float 2. -. 1.
            done ;
        done ;
        AFrame.add_break ab (AFrame.size ab) ;
        remaining <- remaining - (AFrame.size ab) - off ;
        if remaining <= 0 then must_fail <- true

end

let () =
  Lang.add_operator "noise"
    ~category:Lang.Input
    ~descr:"Generate white noise."
    [
      "duration", Lang.float_t, Some (Lang.float 0.), None
    ]
    (fun p ->
       (new noise
          (Lang.to_float (List.assoc "duration" p)) :> source))
