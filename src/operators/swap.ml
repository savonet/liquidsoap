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

open Source

class swap (source:source) chan1 chan2 =
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
        for i = offset to AFrame.position buf -1 do
           let tmp = buffer.(chan1).(i) in
              buffer.(chan1).(i) <- buffer.(chan2).(i);
              buffer.(chan2).(i) <- tmp
        done

end

let () =
  Lang.add_operator "swap"
    [ "chan1", Lang.int_t,  Some (Lang.int 0), Some "Channel one" ;
      "chan2", Lang.int_t,  Some (Lang.int 1), Some "Channel two" ;
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"swap two channels"
    (fun p ->
       let s = Lang.to_source (Lang.assoc "" 1 p) in
       let chan1 = Lang.to_int (Lang.assoc "chan1" 1 p) in
       let chan2 = Lang.to_int (Lang.assoc "chan2" 1 p) in
         ((new swap s chan1 chan2):>source))

