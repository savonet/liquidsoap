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

class setvol (source:source) coeff =
object (self)
  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf ;
      Float_pcm.multiply (AFrame.get_float_pcm buf)
        offset ((AFrame.position buf)-offset) coeff

end

let () = 
  Lang.add_operator "change_volume"
    [ "", Lang.float_t,  None, Some "multiplicative factor" ;
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Scales the amplitude of the signal"
    (fun p ->
       let c = Lang.to_float (Lang.assoc "" 1 p) in
       let s = Lang.to_source (Lang.assoc "" 2 p) in
         ((new setvol s c):>source))
