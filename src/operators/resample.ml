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

class resample (source:source) ratio =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining =
    let rem = source#remaining in
      if rem = -1 then rem else
        int_of_float (float rem *. (ratio ()))

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  val mutable data = Array.make (Fmt.channels()) [||]

  val databuf = Fmt.create_frame ()

  val mutable dpos = 0

  method get_frame buf =
    let b = AFrame.get_float_pcm buf in
      for i = AFrame.position buf to AFrame.size buf - 1 do
        if dpos >= Array.length data.(0) then
          (
            AFrame.clear databuf;
            source#get databuf;
            let db = AFrame.get_float_pcm databuf in
              data <- Float_pcm.resample (ratio ()) db 0 (Array.length db.(0));
              dpos <- 0
          );
        for c = 0 to (Fmt.channels()) - 1 do
          b.(c).(i) <- data.(c).(dpos)
        done;
        dpos <- dpos + 1
      done;
      (* TODO: this operator doesn't seem to handle end of tracks correctly. *)
      AFrame.add_break buf (AFrame.size buf)

end

let () =
  Lang.add_operator "resample"
    [
      "ratio", Lang.float_getter_t 1, None, Some "Conversion ratio";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Resample source's sound using a resampling factor"
    ~flags:[Lang.Hidden;Lang.Experimental]
    (fun p ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let ratio = Lang.to_float_getter (f "ratio") in
         ((new resample src ratio):>source)
    )
