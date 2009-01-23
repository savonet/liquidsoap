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
  Conf.int ~p:(conf#plug "size") ~d:1764
    "Frame size in samples"
    ~comments:[
      "Tweaking this is tricky but can be useful, since the latency is" ;
      "proportional to the frame size. It can also help to get ALSA correctly" ;
      "synchronized with liquidsoap when used in non-buffered mode." ;
      "With video enabled, frame duration should be a multiple of ";
      "audio and video frame duration, in order to maintain synchronization."
    ]
let conf_channels =
  Conf.int ~p:(conf#plug "channels") ~d:2
    "Number of channels"

let conf_video =
  Conf.void ~p:(conf#plug "video") "Video format"
      ~comments:[
        "Parameters for video in liquidsoap."
      ]

(* TODO: better organisation.. *)
let conf_video_channels =
  Conf.int ~p:(conf_video#plug "channels") ~d:0
    "Video channels"
let conf_video_width = 
  Conf.int ~p:(conf_video#plug "width") ~d:320
    "Video frame width"
let conf_video_height =
  Conf.int ~p:(conf_video#plug "height") ~d:240
    "Video frame height"
let conf_video_fps =
  Conf.int ~p:(conf_video#plug "fps") ~d:25
    "Video frames per second"

(* Global time speed. *)
let samples_per_second () = conf_samplerate#get

(* Duration of frames in global time. The name of the setting is biased towards
 * audio... *)
let samples_per_frame () = conf_size#get

(** Number of channels. *)
let channels () = conf_channels#get

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

let video_frames_per_second () = conf_video_fps#get

let video_frames_per_frame () = int_of_float (float (video_frames_per_second ()) *. (seconds_per_frame ()))
let video_frames_of_seconds s = int_of_float (float (video_frames_per_second ()) *. s)
let ticks_of_video_frames f = (ticks_per_second ()) * f / (video_frames_per_second ())
let video_frames_of_ticks t = t / (ticks_per_second () / video_frames_per_second ())

let video_width () = conf_video_width#get
let video_height () = conf_video_height#get

let video_channels () = conf_video_channels#get
