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

class echo (source:source) vmin vmax =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf ;
      let b = AFrame.get_float_pcm buf in
      let position = AFrame.position buf in
        for c = 0 to Array.length b - 1 do
          let bc = b.(c) in
            for i = offset to position - 1 do
              bc.(i) <- max vmin (min vmax bc.(i));
            done
        done
end

let () =
  Lang.add_operator "clip"
    [ "min", Lang.float_t, Some (Lang.float (-0.999)),
      Some "Minimal acceptable value.";
      "max", Lang.float_t, Some (Lang.float 0.999),
      Some "Maximal acceptable value.";
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Clip sound."
    (fun p ->
       let f v = List.assoc v p in
       let vmin, vmax, src =
         Lang.to_float (f "min"),
         Lang.to_float (f "max"),
         Lang.to_source (f "")
       in
         ((new echo src vmin vmax):>source))
