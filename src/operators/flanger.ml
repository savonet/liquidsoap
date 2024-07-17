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

let pi = 3.1416

class flanger (source:source) delay freq feedback =
  let past_len = Fmt.samples_of_seconds delay in
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  val past = Array.init (Fmt.channels()) (fun _ -> Array.make past_len 0.)

  val mutable past_pos = 0

  val mutable omega = 0.

  method get_frame buf =
    let feedback = feedback () in
    let offset = AFrame.position buf in
      source#get buf ;
      let b = AFrame.get_float_pcm buf in
      let position = AFrame.position buf in
      let d_omega = 2. *. pi *. (freq ()) /. (float (Fmt.samples_per_second ())) in
        for i = offset to position - 1 do
          let delay =
            (past_pos + past_len + Fmt.samples_of_seconds (delay *. (1. -. cos omega) /. 2.)) mod past_len
          in
            for c = 0 to Array.length b - 1 do
              past.(c).(past_pos) <- b.(c).(i);
              b.(c).(i) <- (b.(c).(i) +. past.(c).(delay) *. feedback)
                           /. (1. +. feedback);
            done;
          omega <- omega +. d_omega;
          past_pos <- (past_pos + 1) mod past_len
        done
end

let () =
  Lang.add_operator "flanger"
    [ "delay", Lang.float_t, Some (Lang.float 0.001), Some "Delay in seconds.";
      "freq",
      Lang.float_getter_t 1,
      Some (Lang.float 0.5), Some "Frequency in Hz.";
      "feedback",
      Lang.float_getter_t 2,
      Some (Lang.float (0.)), Some "Feedback coefficient in dB.";
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Flanger effect."
    (fun p ->
       let f v = List.assoc v p in
       let duration, freq, feedback, src =
         Lang.to_float (f "delay"),
         Lang.to_float_getter (f "freq"),
         Lang.to_float_getter (f "feedback"),
         Lang.to_source (f "")
       in
       let feedback = fun () -> Sutils.lin_of_dB (feedback ()) in
         ((new flanger src duration freq feedback):>source))
