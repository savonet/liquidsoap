(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Generate a sine *)

open Types

class sine freq =
object
  inherit source

  method is_ready = true

  val mutable must_fail = false
  method abort_track = must_fail <- true

  val mutable phi = 0.

  method get_frame ab =
    if must_fail then begin
      Mixer.Buffer.add_break ab (Mixer.Buffer.position ab) ;
      must_fail <- false
    end else
      let off = Mixer.Buffer.position ab in
	phi <- Mixer.Buffer.sine ab off (Mixer.Buffer.size - off) freq phi;
	Mixer.Buffer.add_break ab Mixer.Buffer.size

  method stype = Infallible
end

let _ =
  Lang.add_operator "sine"
    ~descr:"Plays a boring sine..."
    [ "", Lang.int_t, None, Some "Frequency of the sine" ]
    (fun p ->
       (new sine (Lang.to_int (List.assoc "" p)) :> source))
