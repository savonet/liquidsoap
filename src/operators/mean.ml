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

class mean (source:source) channels =
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
          let mean = List.fold_left (fun m b -> m +. buffer.(b).(i)) 0. channels in
          let mean = mean /. float (List.length channels) in
            List.iter (fun c -> 
                         buffer.(c).(i) <- mean
            ) channels
        done
end

let () =
  Lang.add_operator "mean"
    [
      "channels", Lang.list_t Lang.int_t, Some (Lang.list [Lang.int 0; Lang.int 1]), Some "List of channels to compute the means." ;
      "", Lang.source_t, None, None ;
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Compute the mean of a list of audio channels and use it for all of them."
    (fun p ->
       let s = Lang.to_source (Lang.assoc "" 1 p) in
       let channels = List.map Lang.to_int (Lang.to_list (List.assoc "channels" p)) in
         ((new mean s channels):>source))
