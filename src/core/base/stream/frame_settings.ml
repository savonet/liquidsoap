(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let log = Log.make ["frame"]

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
  Conf.float ~p:(conf#plug "duration") ~d:0.02 "Frame duration in seconds"
    ~comments:
      [
        "Set frame duration, in seconds.";
        "This setting control the latency of the streaming system. When set to";
        "a smaller value, latency is reduced at the expense of more data";
        "consumption. When set to a larger value, CPU and memory usage should";
        "go down but latency should increase.";
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
let conf_video = Conf.void ~p:(conf#plug "video") "Video settings"

let conf_video_default =
  Conf.bool
    ~p:(conf_video#plug "default")
    ~d:false
    "Set to `true` to force video content even when no video content is \
     explicitly requested, for instance: `output.dummy(noise())`"

let conf_video_framerate =
  Conf.int ~p:(conf_video#plug "framerate") ~d:25 "Frame rate"

let conf_video_width =
  Conf.int ~p:(conf_video#plug "width") ~d:1280 "Image width"

let conf_video_height =
  Conf.int ~p:(conf_video#plug "height") ~d:720 "Image height"

let conf_video_detect_dimensions =
  Conf.bool
    ~p:(conf_video#plug "detect_dimensions")
    ~d:true "Automatically detect video dimensions"
    ~comments:
      [
        "When enabled, video dimensions are set from the first decoded video \
         file.";
        "Discarded if `video.frame.width` or `video.frame.height` is set \
         explicitly.";
        "May not work if the video dimensions need to be known before decoding \
         (e.g. with some inline video operators).";
      ]

(* MIDI *)
let conf_midi = Conf.void ~p:(conf#plug "midi") "MIDI parameters"

let conf_midi_channels =
  Conf.int ~p:(conf_midi#plug "channels") ~d:0 "Default number of channels"

(** Format parameters *)

(* The user can set some parameters in the initial configuration script.
   Once we start working with them, changing them again is dangerous.
   Since Dtools doesn't allow that, below is a trick to read the settings
   only once. Later changes will never be taken into account. *)

(* This variable prevents forcing the value of a lazy configuration
   item before the user gets a chance to override the default. *)
let lazy_config_eval = ref false
let delayed_eval = Queue.create ()

let delayed f =
  let ret = Lazy.from_fun f in
  Queue.push (fun () -> ignore (Lazy.force ret)) delayed_eval;
  ret

let () =
  Lifecycle.after_script_parse ~name:"frame settings eval" (fun () ->
      Queue.iter (fun f -> f ()) delayed_eval)

let delayed_conf ~to_string x =
  delayed (fun () ->
      assert !lazy_config_eval;
      let ret = x#get in
      let routes = List.flatten (conf#routes x#ut) in
      log#info "frame.%s set to: %s" (String.concat "." routes) (to_string ret);
      ret)

let ( !! ) = Lazy.force

(** The channel numbers are only defaults, used when channel numbers cannot be
    inferred / are not forced from the context. I'm currently unsure how much
    they are really useful. *)

let audio_channels = delayed_conf ~to_string:string_of_int conf_audio_channels

let default_video_enabled =
  delayed_conf ~to_string:string_of_bool conf_video_default

let midi_channels = delayed_conf ~to_string:string_of_int conf_midi_channels

type ideal_size = { width : int; height : int; source : string }

(** Get video dimensions as a pair of lazy values. Forcing one forces the other.
    If [~ideal_size] is provided and the dimensions haven't been explicitly set
    by the user or forced yet, use the ideal dimensions as the default. This
    allows auto-detection from the first decoded video file. *)
let video_dimensions =
  (* We don't want to use delayed config here because those are evaluated too early. *)
  let dimensions =
    lazy
      (assert !lazy_config_eval;
       let w = conf_video_width#get in
       let h = conf_video_height#get in
       (w, h))
  in
  fun ?ideal_size () ->
    (match ideal_size with
      | Some { width; height; source }
        when conf_video_detect_dimensions#get
             && (not (Lazy.is_val dimensions))
             && (not conf_video_width#is_set)
             && not conf_video_height#is_set ->
          log#important "Auto-detected video dimensions: %dx%d (source: %s)."
            width height source;
          conf_video_width#set width;
          conf_video_height#set height
      | _ -> ());
    let width = lazy (fst (Lazy.force dimensions)) in
    let height = lazy (snd (Lazy.force dimensions)) in
    (width, height)

let audio_rate = delayed_conf ~to_string:string_of_int conf_audio_samplerate
let video_rate = delayed_conf ~to_string:string_of_int conf_video_framerate

(* TODO: midi rate is assumed to be the same as audio,
     so we should not have two different values *)
let midi_rate = delayed_conf ~to_string:string_of_int conf_audio_samplerate

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

(** The main clock is the slowest possible that can convert to both the audio
    and video clocks. *)
let main_rate =
  delayed (fun () ->
      match (!!audio_rate, !!video_rate) with
        | 0, 0 ->
            failwith
              "At least one of \"frame.audio.samplerate\" or \
               \"frame.video.framerate\" should not be 0!"
        | 0, r | r, 0 -> r
        | a, v -> lcm a v)

(** Precompute those ratios to avoid too large integers below. *)
let m_o_a =
  delayed (fun () -> match !!audio_rate with 0 -> 0 | r -> !!main_rate / r)

let m_o_v =
  delayed (fun () -> match !!video_rate with 0 -> 0 | r -> !!main_rate / r)

let main_of_audio a = a * !!m_o_a
let main_of_video v = v * !!m_o_v

(* TODO: for now MIDI rate is the same as audio rate. *)
let main_of_midi = main_of_audio
let audio_of_main m = match !!m_o_a with 0 -> 0 | x -> m / x
let video_of_main m = match !!m_o_v with 0 -> 0 | x -> m / x
let video_of_main_f m = match !!m_o_v with 0 -> 0. | x -> float m /. float x

(* TODO: for now MIDI rate is the same as audio rate. *)
let midi_of_main = audio_of_main
let main_of_seconds d = int_of_float (d *. float !!main_rate)
let audio_of_seconds d = int_of_float (d *. float !!audio_rate)
let video_of_seconds d = int_of_float (d *. float !!video_rate)
let seconds_of_main d = float d /. float !!main_rate
let seconds_of_audio d = float d /. float !!audio_rate
let seconds_of_video d = float d /. float !!video_rate

(** The frame size (in main ticks) should allow for an integer number of samples
    of all types (audio, video). With audio@44100Hz and video@25Hz,
    ticks=samples and one video sample takes 1764 ticks: we need frames of size
    N*1764. *)
let size =
  delayed (fun () ->
      let audio = !!audio_rate in
      let video = !!video_rate in
      let main = !!main_rate in
      log#important "Using %dHz audio, %dHz video, %dHz main." audio video main;
      log#important "Default video frame size: %dx%d%s." conf_video_width#get
        conf_video_height#get
        (if
           conf_video_detect_dimensions#get
           && not (conf_video_width#is_set && conf_video_height#is_set)
         then " (auto-detection enabled)"
         else "");
      try
        let size = main_of_audio conf_audio_size#get in
        log#important
          "Targeting 'frame.audio.size': %d audio samples = %d ticks = %.2fs."
          conf_audio_size#get size (seconds_of_main size);
        size
      with Conf.Undefined _ ->
        let size = main_of_seconds conf_duration#get in
        log#important "Targeting 'frame.duration': %.2fs = %d ticks."
          conf_duration#get size;
        size)

let duration = delayed (fun () -> float !!size /. float !!main_rate)
