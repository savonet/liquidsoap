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

class compand (source:source) mu =
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
            (* Cf. http://en.wikipedia.org/wiki/Mu-law *)
            let sign = if b.(c).(i) < 0. then -1. else 1. in
              b.(c).(i) <- sign *. log (1. +. mu  *. abs_float b.(c).(i)) /. log (1. +. mu)
          done
        done
end

let () =
  Lang.add_operator "compand"
    [
      "mu", Lang.float_t, Some (Lang.float 1.), None;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Compand the signal"
    (fun p ->
       let f v = List.assoc v p in
       let mu, src =
         Lang.to_float (f "mu"),
         Lang.to_source (f "")
       in
         ((new compand src mu):>source)
    )
