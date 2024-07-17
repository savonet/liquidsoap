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

(* See http://en.wikipedia.org/wiki/Comb_filter *)

class comb (source:source) delay feedback =
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
      let feedback = feedback () in
        for i = offset to position - 1 do
          for c = 0 to Array.length b - 1 do
            let oldin = b.(c).(i) in
              b.(c).(i) <- b.(c).(i) +. past.(c).(past_pos) *. feedback;
              past.(c).(past_pos) <- oldin
          done;
          past_pos <- (past_pos + 1) mod past_len
        done
end

let () =
  Lang.add_operator "comb"
    [ "delay", Lang.float_t, Some (Lang.float 0.001), Some "Delay in seconds.";
      "feedback", Lang.float_getter_t 1, Some (Lang.float (-6.)), Some "Feedback coefficient in dB.";
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Comb filter."
    (fun p ->
       let f v = List.assoc v p in
       let duration, feedback, src =
         Lang.to_float (f "delay"),
         Lang.to_float_getter (f "feedback"),
         Lang.to_source (f "")
       in
         ((new comb src duration (fun () -> Sutils.lin_of_dB (feedback ()))):>source))
