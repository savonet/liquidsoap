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

open Source

let pi = 3.14159265358979323846

class pan (source:source) phi phi_0 =
object (self)
  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf ;
      let buffer = AFrame.get_float_pcm buf in
      (* Degrees to radians + half field. *)
      let phi_0 = (phi_0 ()) *. pi /. 360. in
       (* Map -1 / 1 to radians. *)
      let phi = (phi ()) *. phi_0 in
      let gain_left = ((tan phi_0) +. tan phi) /. 2. in
      let gain_right = ((tan phi_0) -. tan phi) /. 2. in
        for i = offset to AFrame.position buf -1 do
          buffer.(0).(i) <- buffer.(0).(i) *. gain_left;
          buffer.(1).(i) <- buffer.(1).(i) *. gain_right
        done
end

let () =
  Lang.add_operator "stereo.pan"
    [ "pan", Lang.float_getter_t 1,  Some (Lang.float 0.),
      Some "Pan ranges between -1 and 1." ;
      "field", Lang.float_getter_t 2,  Some (Lang.float 90.),
      Some "Field width in degrees (between 0 and 90)." ;
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Pan a stereo sound."
    (fun p ->
       let s = Lang.to_source (Lang.assoc "" 1 p) in
       let phi_0 = Lang.to_float_getter (Lang.assoc "field" 1 p) in
       let phi = Lang.to_float_getter (Lang.assoc "pan" 1 p) in
         ((new pan s phi phi_0):>source))
