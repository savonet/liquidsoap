(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

exception Added_break

class resample (source:source) ratio =
  let fst (x,_,_) = x in
  let snd (_,x,_) = x in
  let trd (_,_,x) = x in
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining =
    let rem = source#remaining in
      if rem = -1 then rem else
        int_of_float (float rem *. (ratio ()))

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  (* Data is: (pcm data, metadata, breaks) *)
  val mutable data = (Array.make (Fmt.channels()) [||],[],[])

  val databuf = Frame.make ()

  val converter = Audio_converter.Samplerate.create (Fmt.channels ())

  val mutable dpos = 0

  method private get_frame buf =
    let b = AFrame.get_float_pcm buf in
     try
      for i = AFrame.position buf to AFrame.size buf - 1 do
        let audio = fst data in
        if dpos >= Array.length audio.(0) then
          (
            AFrame.advance databuf;
            if source#is_ready then
              source#get databuf
            else
              begin
                AFrame.add_break buf i;
                data <- (Array.make (Fmt.channels()) [||],[],[]);
                dpos <- 0;
                raise Added_break
              end;
            let ratio = ratio () in
            let db = AFrame.get_float_pcm databuf in
              let audio =  Audio_converter.Samplerate.resample converter
                         ratio db 0 (Array.length db.(0))
              in
              (* Add metadata and breaks (not end of frame breaks) *)
              let meta = AFrame.get_all_metadata databuf in
              let meta = 
                List.map (fun (x,y) -> int_of_float (float x /. ratio),y) meta 
              in
              let breaks = 
                List.filter (fun x -> x < AFrame.size databuf)
                            (AFrame.breaks databuf)
              in
              let breaks =
                List.map (fun x -> int_of_float (float x /. ratio)) breaks 
              in
              data <- (audio,meta,breaks);
              dpos <- 0
          );
        let audio = fst data in
        let metadata = snd data in
        let breaks = trd data in
        for c = 0 to (Fmt.channels()) - 1 do
          b.(c).(i) <- audio.(c).(dpos)
        done;
        if List.mem_assoc dpos metadata then
          AFrame.set_metadata buf i (List.assoc dpos metadata);
        dpos <- dpos + 1;
        (** Add break. Stop loop in this case. *)
        if List.mem (dpos-1) breaks then
         begin
          AFrame.add_break buf i;
          raise Added_break
         end
      done;
      AFrame.add_break buf (AFrame.size buf)
     with
       | Added_break -> ()

end

let () =
  Lang.add_operator "resample"
    [
      "ratio", Lang.float_getter_t 1, None, Some "Conversion ratio";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Resample source's sound using a resampling factor"
    (fun p _ ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let ratio = Lang.to_float_getter (f "ratio") in
         new resample src ratio)
