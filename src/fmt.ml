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

(* Global time speed. *)
let samples_per_second () = Dtools.Conf.get_int ~default:44100 "frame.samplerate"

(* Duration of frames in global time. The name of the setting is biased towards
 * audio... *)
let samples_per_frame () = Dtools.Conf.get_int ~default:1024 "frame.size"

(** Number of channels. *)
let channels () = Dtools.Conf.get_int ~default:2 "frame.channels"

(* Create a standard audio frame. *)
let create_frame () =
  Frame.create
    (Array.init (channels())
       (fun _ ->
          Frame.Float_pcm_t (float (samples_per_second()))))
    (samples_per_second()) (samples_per_frame())

(* Some day the time reference might not be the standard audio sample. *)
let ticks_per_second = samples_per_second
let ticks_per_frame = samples_per_frame

(** Sample length in ticks. *)
let ticks_per_sample () = (ticks_per_second()) / (samples_per_second())
let seconds_per_frame () = float (ticks_per_frame()) /. float (ticks_per_second())

let samples_of_seconds s = int_of_float (s *. float (samples_per_second()))
let ticks_of_seconds s = int_of_float (s *. float (ticks_per_second()))
let ticks_of_samples s = s * (ticks_per_second() / samples_per_second())
let samples_of_ticks t = t / (ticks_per_second() / samples_per_second())
let seconds_of_samples i = float i /. float (samples_per_second ())
let seconds_of_ticks i   = float i /. float (ticks_per_second ())
