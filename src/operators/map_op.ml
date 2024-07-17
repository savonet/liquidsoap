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

open Source

class map source f =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.get_float_pcm buf in
        for i = offset to AFrame.position buf - 1 do
          for c = 0 to Array.length b - 1 do
            b.(c).(i) <- f b.(c).(i)
          done
        done
end

let to_fun_float f x =
  Lang.to_float (Lang.eval (Lang.app f ["", Lang.float x]))

let () =
  Lang.add_operator "map"
    [
      "", Lang.fun_t [false,"",Lang.float_t] Lang.float_t, None, None;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Map a function on the sound."
    ~flags:[Lang.Hidden; Lang.Experimental]
    (fun p ->
       let f, src =
         to_fun_float (Lang.assoc "" 1 p),
         Lang.to_source (Lang.assoc "" 2 p)
       in
         ((new map src f):>source)
    )
