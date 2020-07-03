(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** Configuration entries *)

module Conf = Dtools.Conf

let conf =
  Conf.void
    ~p:(Configure.conf#plug "frame")
    "Frame format"
    ~comments:
      [
        "Settings for the data representation in frames, which are the";
        "elementary packets of which streams are made.";
      ]

let conf_duration =
  Conf.float ~p:(conf#plug "duration") ~d:0.04
    "Tentative frame duration in seconds"
    ~comments:
      [
        "Audio samplerate and video frame rate constrain the possible frame \
         durations.";
        "This setting is used as a hint for the duration, when \
         'frame.audio.size'";
        "is not provided.";
        "Tweaking frame duration is tricky but needed when dealing with latency";
        "or getting soundcard I/O correctly synchronized with liquidsoap.";
      ]

(* Audio *)
let conf_audio = Conf.void ~p:(conf#plug "audio") "Audio (PCM) format"

let conf_audio_samplerate =
  Conf.int ~p:(conf_audio#plug "samplerate") ~d:44100 "Samplerate"

let conf_audio_channels =
  Conf.int ~p:(conf_audio#plug "channels") ~d:2 "Default number of channels"

let conf_audio_size =
  Conf.int ~p:(conf_audio#plug "size")
    "Tentative frame duration in audio samples"
    ~comments:
      [
        "Audio samplerate and video frame rate constrain the possible frame \
         durations.";
        "This setting is used as a hint for the duration, overriding";
        "'frame.duration'.";
        "Tweaking frame duration is tricky but needed when dealing with latency";
        "or getting soundcard I/O correctly synchronized with liquidsoap.";
      ]

(* Video *)
let conf_video =
  Conf.bool ~p:(conf#plug "video") ~d:false "Enable video support"

let conf_video_framerate =
  Conf.int ~p:(conf_video#plug "framerate") ~d:25 "Frame rate"

let conf_video_width =
  Conf.int ~p:(conf_video#plug "width") ~d:1280 "Image width"

let conf_video_height =
  Conf.int ~p:(conf_video#plug "height") ~d:720 "Image height"

(* MIDI *)
let conf_midi = Conf.void ~p:(conf#plug "midi") "MIDI parameters"

let conf_midi_channels =
  Conf.int ~p:(conf_midi#plug "channels") ~d:0 "Default number of channels"

(** Format parameters *)

(* The user can set some parameters in the initial configuration script.
 * Once we start working with them, changing them again is dangerous.
 * Since Dtools doesn't allow that, below is a trick to read the settings
 * only once. Later changes will never be taken into account. *)

(* This variable prevents forcing the value of a lazy configuration
 * item before the user gets a chance to override the default. *)
let lazy_config_eval = ref false
let allow_lazy_config_eval () = lazy_config_eval := true
let delayed f = lazy (f ())

let delayed_conf x =
  delayed (fun () ->
      assert !lazy_config_eval;
      x#get)

let ( !! ) = Lazy.force

(** The channel numbers are only defaults, used when channel numbers
  * cannot be inferred / are not forced from the context.
  * I'm currently unsure how much they are really useful. *)

let audio_channels = delayed_conf conf_audio_channels
let video_enabled = delayed_conf conf_video
let midi_channels = delayed_conf conf_midi_channels
let video_width = delayed_conf conf_video_width
let video_height = delayed_conf conf_video_height
let audio_rate = delayed_conf conf_audio_samplerate
let video_rate = delayed_conf conf_video_framerate

(* TODO: midi rate is assumed to be the same as audio,
 *   so we should not have two different values *)
let midi_rate = delayed_conf conf_audio_samplerate

(** Greatest common divisor. *)
let rec gcd a b =
  match compare a b with
    | 0 (* a=b *) -> a
    | 1 (* a>b *) -> gcd (a - b) b
    | _ (* a<b *) -> gcd a (b - a)

(** Least common multiplier. *)
let lcm a b = a / gcd a b * b

(* divide early to avoid overflow *)

(** [upper_multiple k m] is the least multiple of [k] that is [>=m]. *)
let upper_multiple k m = if m mod k = 0 then m else (1 + (m / k)) * k

(** The master clock is the slowest possible that can convert to both
  * the audio and video clocks. *)
let master_rate = delayed (fun () -> lcm !!audio_rate !!video_rate)

(** Precompute those ratios to avoid too large integers below. *)
let m_o_a = delayed (fun () -> !!master_rate / !!audio_rate)

let m_o_v = delayed (fun () -> !!master_rate / !!video_rate)
let master_of_audio a = a * !!m_o_a
let master_of_video v = v * !!m_o_v

(* TODO: for now MIDI rate is the same as audio rate. *)
let master_of_midi = master_of_audio
let audio_of_master m = m / !!m_o_a
let video_of_master m = m / !!m_o_v

(* TODO: for now MIDI rate is the same as audio rate. *)
let midi_of_master = audio_of_master
let master_of_seconds d = int_of_float (d *. float !!master_rate)
let audio_of_seconds d = int_of_float (d *. float !!audio_rate)
let video_of_seconds d = int_of_float (d *. float !!video_rate)
let seconds_of_master d = float d /. float !!master_rate
let seconds_of_audio d = float d /. float !!audio_rate
let seconds_of_video d = float d /. float !!video_rate
let log = Log.make ["frame"]

(** The frame size (in master ticks) should allow for an integer
  * number of samples of all types (audio, video).
  * With audio@44100Hz and video@25Hz, ticks=samples and one video
  * sample takes 1764 ticks: we need frames of size N*1764. *)
let size =
  delayed (fun () ->
      let audio = !!audio_rate in
      let video = !!video_rate in
      let master = !!master_rate in
      let granularity = lcm (master / audio) (master / video) in
      let target =
        log#important "Using %dHz audio, %dHz video, %dHz master." audio video
          master;
        log#important
          "Frame size must be a multiple of %d ticks = %d audio samples = %d \
           video samples."
          granularity
          (audio_of_master granularity)
          (video_of_master granularity);
        try
          let d = conf_audio_size#get in
          log#important
            "Targetting 'frame.audio.size': %d audio samples = %d ticks." d
            (master_of_audio d);
          master_of_audio d
        with Conf.Undefined _ ->
          log#important
            "Targetting 'frame.duration': %.2fs = %d audio samples = %d ticks."
            conf_duration#get
            (audio_of_seconds conf_duration#get)
            (master_of_seconds conf_duration#get);
          master_of_seconds conf_duration#get
      in
      let s = upper_multiple granularity (max 1 target) in
      log#important
        "Frames last %.2fs = %d audio samples = %d video samples = %d ticks."
        (seconds_of_master s) (audio_of_master s) (video_of_master s) s;
      s)

let duration = delayed (fun () -> float !!size /. float !!master_rate)
