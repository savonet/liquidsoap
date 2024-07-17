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

module Conf = Dtools.Conf

let conf =
  Conf.void ~p:(Configure.conf#plug "frame") "Frame format"
    ~comments:[
      "Internally, liquidsoap handles raw audio. These streams are processed" ;
      "in a packetized way, frame by frame." ;
      "Here are some parameters of the internal audio format and packetizing."
    ]
let conf_samplerate =
  Conf.int ~p:(conf#plug "samplerate") ~d:44100
    "Internal samplerate"
let conf_size =
  Conf.int ~p:(conf#plug "size") ~d:1024
    "Frame size in samples"
    ~comments:[
      "Tweaking this is tricky but can be useful, since the latency is" ;
      "proportional to the frame size. It can also help to get ALSA correctly" ;
      "synchronized with liquidsoap when used in non-buffered mode."
    ]
let conf_channels =
  Conf.int ~p:(conf#plug "channels") ~d:2
    "Number of channels"

(* Global time speed. *)
let samples_per_second () = conf_samplerate#get

(* Duration of frames in global time. The name of the setting is biased towards
 * audio... *)
let samples_per_frame () = conf_size#get

(** Number of channels. *)
let channels () = conf_channels#get

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
