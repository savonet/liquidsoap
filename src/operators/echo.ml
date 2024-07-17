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

let clip x = min 1. (max (-1.) x)

class echo (source:source) delay feedback =
  let past_len = Fmt.samples_of_seconds delay in
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  val past = Array.init (Fmt.channels()) (fun _ -> Array.make past_len 0.)

  val mutable past_pos = 0

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf ;
      let b = AFrame.get_float_pcm buf in
      let position = AFrame.position buf in
        for i = offset to position - 1 do
          for c = 0 to Array.length b - 1 do
            b.(c).(i) <- clip (b.(c).(i) +. past.(c).(past_pos) *. feedback);
            past.(c).(past_pos) <- b.(c).(i)
          done;
          past_pos <- (past_pos + 1) mod past_len
        done
end

let () =
  Lang.add_operator "echo"
    [ "delay", Lang.float_t, Some (Lang.float 0.5), Some "delay in seconds";
      "feedback", Lang.float_t, Some (Lang.float (-6.)), Some "feedback coefficient in dB (<= 0)";
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Add echo"
    (fun p ->
       let f v = List.assoc v p in
       let duration, feedback, src =
         Lang.to_float (f "delay"),
         Lang.to_float (f "feedback"),
         Lang.to_source (f "")
       in
         if feedback > 0. then raise (Lang.Invalid_value (f "feedback", "feedback should be <=0"));
         ((new echo src duration (Sutils.lin_of_dB feedback)):>source))
