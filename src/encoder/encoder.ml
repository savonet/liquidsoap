(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

(** Common infrastructure for encoding streams *)

let conf = 
  Dtools.Conf.void ~p:(Configure.conf#plug "encoder") "Encoder settings"
    ~comments:["Settings for the encoder"]

let conf_meta =
  Dtools.Conf.void ~p:(conf#plug "encoder") "Metadata settings"
    ~comments:["Settings for the encoded metadata."]

(** The list of metadata fields that should be exported when encoding. *)
let conf_export_metadata =
  Dtools.Conf.list ~p:(conf_meta#plug "export") "Exported metdata"
    ~d:["artist";"title";"album";"genre";"date";"tracknumber";
    "comment";"track";"year"]
    ~comments:["The list of labels of exported metadata."]

(* This is because I am too lazy to write encoder.mli.. *)
module Meta : 
  sig
    (* I would like to use a private 
     * type here but its only for ocaml >= 3.11.. *)
    type export_metadata
    val export_metadata : Frame.metadata -> export_metadata
    val to_metadata : export_metadata -> Frame.metadata
    val empty_metadata : export_metadata
    val is_empty : export_metadata -> bool
  end = 
  struct
    type export_metadata = Frame.metadata
    let export_metadata m =
      let ret = Hashtbl.create 10 in
      let l = conf_export_metadata#get in
      Hashtbl.iter (fun x y -> if List.mem (Utils.StringCompat.lowercase_ascii x) l then
                               Hashtbl.add ret x y) m;
    ret
    let to_metadata m = m
    let empty_metadata = Hashtbl.create 0
    let is_empty m = Hashtbl.length m == 0
  end

let string_of_stereo s =
  if s then "stereo" else "mono"

module WAV =
struct

  type t = { samplerate : int;
             samplesize : int;
             channels   : int;
             duration   : float option;
             header     : bool }

  let to_string w =
    let duration = 
      match w.duration with
        | None -> ""
        | Some d -> Printf.sprintf ",duration=%f" d
    in
    Printf.sprintf
      "%%wav(samplerate=%d,channels=%d,samplesize=%d,header=%b%s)"
      w.samplerate w.channels w.samplesize w.header duration

end

module AVI =
struct
  type t =
    {
      samplerate : int;
      channels : int;
    }

  let to_string w =
    Printf.sprintf
      "%%avi(samplerate=%d,channels=%d)"
      w.samplerate w.channels
end

module Vorbis =
struct

  type quality = float
  type bitrate = int
  type mode =
    | VBR of quality                 (* Variable bitrate. *)
    | CBR of bitrate                 (* Constant bitrate. *)
    | ABR of (bitrate option)*(bitrate option)*(bitrate option) (* Average: min,avg,max. *)

  type t = {
    channels   : int ;
    mode       : mode ;
    samplerate : int ;
    fill       : int option ;
  }

  let string_of_mode = function
    | ABR (min,avg,max) ->
        let f v x =
          match x with
            | Some x -> Printf.sprintf "%s=%d," v x
            | None   -> ""
        in
        Printf.sprintf ".abr(%s%s%s" (f "min_bitrate" min)
                                     (f "bitrate" avg)
                                     (f "max_bitrate" max)
    | CBR bitrate ->
        Printf.sprintf ".cbr(bitrate=%d" bitrate
    | VBR q ->
        Printf.sprintf "(quality=%.2f" q

  let to_string v =
    Printf.sprintf "%%vorbis%s,channels=%d,samplerate=%d)"
      (string_of_mode v.mode)
      v.channels
      v.samplerate
end

module Opus =
struct
  type application = [
    | `Voip
    | `Audio
    | `Restricted_lowdelay
  ]

  type bitrate = [
    | `Auto
    | `Bitrate_max
    | `Bitrate of int
  ]

  type mode =
    | VBR of bool        (* Variable bitrate, constrained or not. *)
    | CBR                (* Constant bitrate. *)

  type max_bandwidth = [
    | `Narrow_band
    | `Medium_band
    | `Wide_band
    | `Super_wide_band
    | `Full_band 
  ]

  type signal = [
    | `Auto
    | `Voice
    | `Music
  ]

  type t = {
    application   : application option ;
    bitrate       : bitrate ;
    complexity    : int option ;
    channels      : int ;
    frame_size    : float ;
    max_bandwidth : max_bandwidth option ;
    mode          : mode ;
    samplerate    : int ;
    signal        : signal option ;
    fill          : int option ;
    dtx           : bool ;
  }

  let string_of_bitrate = function
    | `Auto -> "birate=\"auto\","
    | `Bitrate_max -> "birate=\"max\","
    | `Bitrate b -> Printf.sprintf "bitrate=%d," b

  let string_of_mode = function
    | CBR   -> "vbr=\"none\""
    | VBR b -> Printf.sprintf "vbr=%S" (if b then "constrained" else "unconstrained")

  let string_of_application = function
    | None -> ""
    | Some `Voip  -> "application=\"voip\","
    | Some `Audio -> "application=\"audio\","
    | Some `Restricted_lowdelay -> "application=\"restricted_lowdelay\","

  let string_of_bandwidth = function
    | None -> ""
    | Some `Narrow_band -> "max_bandwidth=\"narrow_band\","
    | Some `Medium_band -> "max_bandwidth=\"medium_band\","
    | Some `Wide_band   -> "max_bandwidth=\"wide_band\","
    | Some `Super_wide_band -> "max_bandwidth=\"super_wide_band\","
    | Some `Full_band -> "max_bandwidth=\"full_band\","

  let string_of_signal = function
    | None -> ""
    | Some `Auto -> "signal=\"auto\","
    | Some `Voice -> "signal=\"voice\","
    | Some `Music -> "signal=\"music\","

  let to_string v =
    Printf.sprintf
    "%%opus(%s,%schannels=%d,%s%s%s%ssamplerate=%d,frame_size=%.02f,dtx=%B)"
      (string_of_mode v.mode)
      (string_of_bitrate v.bitrate)
      v.channels
      (string_of_application v.application)
      (match v.complexity with None -> "" | Some i -> (Printf.sprintf "complexity=\"%d\"," i))
      (string_of_bandwidth v.max_bandwidth)
      (string_of_signal v.signal)
      v.samplerate
      v.frame_size
      v.dtx
end

module MP3 =
struct
  type stereo_mode = 
    | Default
    | Stereo
    | Joint_stereo

  type abr =   
    { min_bitrate  : int option ;
      mean_bitrate : int ;
      max_bitrate  : int option ;
      hard_min     : bool }

  let string_of_abr x = 
    let f v x = 
      match x with
        | Some x -> Printf.sprintf "%s=%i," v x
        | None   -> ""
    in
    Printf.sprintf "bitrate=%d,%s%shard_min=%b"
      x.mean_bitrate
      (f "min_bitrate" x.min_bitrate)
      (f "max_bitrate" x.max_bitrate)
      x.hard_min

  type bitrate_control = ABR of abr | VBR of int | CBR of int

  let string_of_bitrate_control =
    function
       | ABR abr -> string_of_abr abr
       | VBR q   -> Printf.sprintf "quality=%d" q
       | CBR br  -> Printf.sprintf "bitrate=%d" br

  type id3v2_export = Meta.export_metadata -> string

  type t = {
    stereo           : bool ;
    stereo_mode      : stereo_mode ;
    bitrate_control  : bitrate_control ;
    internal_quality : int ;
    samplerate       : int ;
    id3v2            : id3v2_export option ;
    msg_interval     : float ;
    msg              : string
  }

  let id3v2_export : id3v2_export option ref = ref None

  let to_string m =
    let name = 
    match m.bitrate_control with
      | VBR _ -> "%mp3.vbr"
      | ABR _ -> "%mp3.abr"    
      | CBR _ -> "%mp3"
    in
    Printf.sprintf "%s(%s,%s,samplerate=%d,id3v2=%b)"
      name
      (string_of_stereo m.stereo)
      (string_of_bitrate_control m.bitrate_control)
      m.samplerate
      (m.id3v2 <> None)

end

module Flac =
struct

  type t = {
    channels : int ;
    bits_per_sample : int ;
    samplerate : int ;
    compression : int ;
    fill : int option ;
  }

  let to_string m =
    Printf.sprintf
      "%%flac(channels=%i,bits_per_sample=%i,samplerate=%d,compression=%i)"
      m.channels m.bits_per_sample m.samplerate m.compression

end

module Shine =
struct

  type t = {
    channels   : int ;
    samplerate : int ;
    bitrate    : int ;
  }

  let to_string m =
    Printf.sprintf "%%shine(channels=%d,samplerate=%d,bitrate=%d)"
      m.channels
      m.samplerate
      m.bitrate

end

module AACPlus =
struct

  type t = {
    channels   : int ;
    samplerate : int ;
    bitrate    : int ;
  }

  let to_string m =
    Printf.sprintf "%%aac+(channels=%d,samplerate=%d,bitrate=%d)"
      m.channels
      m.samplerate
      m.bitrate

end

module FdkAacEnc =
struct
  type mpeg2_aac =
    [
       | `AAC_LC
       | `HE_AAC
       | `HE_AAC_v2
    ]

  type mpeg4_aac =
    [
       | mpeg2_aac
       | `AAC_LD
       | `AAC_ELD
    ]

  type aot =
    [
       | `Mpeg_4 of mpeg4_aac
       | `Mpeg_2 of mpeg2_aac
    ]

  type bitrate_mode =
    [
       | `Constant
       | `Variable of int
    ]

  type transmux =
    [
       | `Raw
       | `Adif
       | `Adts
       | `Latm
       | `Latm_out_of_band
       | `Loas
    ]

  type t = {
    afterburner    : bool;
    aot            : aot;
    bitrate_mode   : bitrate_mode;
    bitrate        : int;
    channels       : int;
    samplerate     : int;
    sbr_mode       : bool;
    transmux       : transmux
  }

  let string_of_aot = function
    | `Mpeg_4 `AAC_LC -> "mpeg4_aac_lc"
    | `Mpeg_4 `HE_AAC -> "mpeg4_he_aac"
    | `Mpeg_4 `HE_AAC_v2 -> "mpeg4_he_aac_v2"
    | `Mpeg_4 `AAC_LD -> "mpeg4_aac_ld"
    | `Mpeg_4 `AAC_ELD -> "mpeg4_aac_eld"
    | `Mpeg_2 `AAC_LC -> "mpeg2_aac_lc"
    | `Mpeg_2 `HE_AAC -> "mpeg2_he_aac"
    | `Mpeg_2 `HE_AAC_v2 -> "mpeg2_he_aac_v2"

  let aot_of_string = function
    | "mpeg4_aac_lc" -> `Mpeg_4 `AAC_LC
    | "mpeg4_he_aac" -> `Mpeg_4 `HE_AAC 
    | "mpeg4_he_aac_v2" -> `Mpeg_4 `HE_AAC_v2
    | "mpeg4_aac_ld" -> `Mpeg_4 `AAC_LD
    | "mpeg4_aac_eld" -> `Mpeg_4 `AAC_ELD
    | "mpeg2_aac_lc" -> `Mpeg_2 `AAC_LC
    | "mpeg2_he_aac" -> `Mpeg_2 `HE_AAC
    | "mpeg2_he_aac_v2" -> `Mpeg_2 `HE_AAC_v2
    | _ -> raise Not_found

  let string_of_transmux = function
    | `Raw -> "raw"
    | `Adif -> "adif"
    | `Adts -> "adts"
    | `Latm -> "latm"
    | `Latm_out_of_band -> "latm_out_of_band"
    | `Loas -> "loas"

  let transmux_of_string = function
    | "raw" -> `Raw
    | "adif" -> `Adif
    | "adts" -> `Adts
    | "latm" -> `Latm
    | "latm_out_of_band" -> `Latm_out_of_band
    | "loas" -> `Loas
    | _ -> raise Not_found

  let to_string m =
    let br_info =
      match m.bitrate_mode with
        | `Variable vbr -> Printf.sprintf "vbr=%d" vbr
        | `Constant     -> Printf.sprintf "bitrate=%d" m.bitrate
    in 
    Printf.sprintf "%%fdkaac(afterburner=%b,aot=%S,%s,channels=%d,\
                             samplerate=%d,sbr_mode=%b,transmux=%S)"
      m.afterburner (string_of_aot m.aot) br_info m.channels
      m.samplerate m.sbr_mode (string_of_transmux m.transmux)
end

module VoAacEnc =
struct

  type t = {
    channels   : int ;
    samplerate : int ;
    bitrate    : int ;
    adts       : bool ;
  }

  let to_string m =
    Printf.sprintf "%%aac(channels=%d,samplerate=%d,bitrate=%d,adts=%b)"
      m.channels
      m.samplerate
      m.bitrate
      m.adts
end

module External =
struct

  exception No_process

  type restart_condition = Delay of int | Metadata | No_condition

  type t = {
    channels            : int ;
    samplerate          : int ;
    video               : bool ;
    header              : bool ;
    restart_on_crash    : bool ;
    restart             : restart_condition ;
    process             : string
  }

  let to_string e =
    let string_of_restart_condition c =
      match c with
        | Delay d         -> Printf.sprintf "restart_after_delay=%i" d
        | Metadata        -> "restart_on_metadata"
        | No_condition    -> ""
    in
    Printf.sprintf "%%external(channels=%i,samplerate=%i,video=%b,header=%b,\
                              restart_on_crash=%b,%s,process=%s)"
      e.channels
      e.samplerate
      e.video
      e.header
      e.restart_on_crash
      (string_of_restart_condition e.restart)
      e.process

end

module Speex =
struct

  type bitrate_control = Quality of int | Vbr of int | Abr of int
  type mode            = Narrowband | Wideband | Ultra_wideband

  type t = {
    bitrate_control   : bitrate_control ;
    samplerate        : int ;
    stereo            : bool ;
    mode              : mode ;
    frames_per_packet : int ;
    complexity        : int option ;
    fill              : int option ;
    dtx               : bool ;
    vad               : bool ;
  }

  let string_of_br_ctl x =
    match x with
      | Vbr x -> Printf.sprintf "vbr,quality=%d" x
      | Abr x -> Printf.sprintf "abr,bitrate=%d" x
      | Quality x -> Printf.sprintf "quality=%d" x

  let string_of_mode x =
    match x with
      | Narrowband -> "narrowband"
      | Wideband   -> "widebande"
      | Ultra_wideband -> "ultra-wideband"

  let string_of_complexity x =
    match x with
      | None -> ""
      | Some x -> Printf.sprintf ",complexity=%d" x

  let to_string m =
    Printf.sprintf
      "%%speex(%s,%s,samplerate=%d,mode=%s,frames_per_packet=%d%s,dtx=%B,vad=%B)"
      (string_of_stereo m.stereo)
      (string_of_br_ctl m.bitrate_control)
      m.samplerate
      (string_of_mode m.mode)
      m.frames_per_packet
      (string_of_complexity m.complexity)
      m.dtx
      m.vad

end

module GStreamer =
struct
  type t = {
    channels  : int;
    audio     : string option;
    has_video : bool;
    video     : string option;
    muxer     : string option;
    metadata  : string;
    pipeline  : string option;
    log       : int
  }

  let audio_channels m =
    if m.audio = None then
      0
    else
      m.channels

  let video_channels m =
    if m.video = None || not m.has_video then
      0
    else
      1

  let to_string m =
    let pipeline l name value = 
      Utils.some_or l
        (Utils.maybe
          (fun value -> (Printf.sprintf "%s=%S" name value)::l)
            value)
    in
    Printf.sprintf "%%gstreamer(%s,metadata=%S,has_video=%b,%slog=%d)"
      (String.concat ","
        (pipeline
         (pipeline
           (pipeline [Printf.sprintf "channels=%d" m.channels] 
             "audio" m.audio)
             "video" m.video)
             "muxer" m.muxer))
      m.metadata
      m.has_video
      (Utils.some_or "" (Utils.maybe (Printf.sprintf "pipeline=%S,") m.pipeline))
      m.log
end

module Theora =
struct

  type bitrate_control = Quality of int | Bitrate of int

  type t = {
    (** TODO: framerate ! *)
    bitrate_control    : bitrate_control ;
    width              : int Lazy.t ;
    height             : int Lazy.t ;
    picture_width      : int Lazy.t ;
    picture_height     : int Lazy.t ;
    picture_x          : int ;
    picture_y          : int ;
    aspect_numerator   : int ;
    aspect_denominator : int ;
    keyframe_frequency : int ;
    vp3_compatible     : bool option ;
    soft_target        : bool ;
    buffer_delay       : int option ;
    speed              : int option ;
    fill               : int option ;
  }

  let bit_ctl_to_string bit_ctl =
    match bit_ctl with
      | Quality x -> Printf.sprintf "quality=%d" x
      | Bitrate x -> Printf.sprintf "bitrate=%d" x

  let print_some_bool v x = 
    match x with
      | None -> ""
      | Some x -> Printf.sprintf "%s=%b" v x

  let print_some_int v x =
    match x with
      | None -> ""
      | Some x -> Printf.sprintf "%s=%i" v x

  let to_string th =
    let f = Lazy.force in
    Printf.sprintf "%%theora(%s,width=%d,height=%d,picture_width=%d,\
                           picture_height=%d,picture_x=%d,picture_y=%d,\
                           aspect_numerator=%d,aspect_denominator=%d,\
                           keyframe_frequence=%d,%s,\
                           soft_target=%b,%s,%s)"
    (bit_ctl_to_string th.bitrate_control) (f th.width) (f th.height)
    (f th.picture_width) (f th.picture_height) th.picture_x th.picture_y
    th.aspect_numerator th.aspect_denominator 
    th.keyframe_frequency 
    (print_some_bool "vp3_compatible" th.vp3_compatible) 
    th.soft_target
    (print_some_int "buffer_delay" th.buffer_delay)
    (print_some_int "speed" th.speed)

end

module Dirac =
struct

  type t = {
    (** TODO: framerate ! *)
    quality            : float ;
    width              : int Lazy.t ;
    height             : int Lazy.t ;
    aspect_numerator   : int ;
    aspect_denominator : int ;
    fill               : int option ;
  }

  let to_string dr =
    let f = Lazy.force in
    Printf.sprintf "%%dirac(quality=%02f,width=%d,height=%d,\
                            aspect_numerator=%d,aspect_denominator=%d)"
    dr.quality (f dr.width) (f dr.height)
    dr.aspect_numerator dr.aspect_denominator

end

module Ogg =
struct

  type item =
    | Speex of Speex.t
    | Vorbis of Vorbis.t
    | Flac of Flac.t
    | Theora of Theora.t
    | Dirac of Dirac.t
    | Opus of Opus.t
  type t = item list

  let to_string l =
    Printf.sprintf "%%ogg(%s)"
      (String.concat ","
         (List.map
            (function
               | Vorbis v -> Vorbis.to_string v
               | Flac   v -> Flac.to_string v
               | Theora t -> Theora.to_string t
               | Speex  s -> Speex.to_string s
               | Dirac  d -> Dirac.to_string d
               | Opus   o -> Opus.to_string o)
            l))

end

type format =
  | WAV of WAV.t
  | AVI of AVI.t
  | Ogg of Ogg.t
  | MP3 of MP3.t
  | Shine of Shine.t
  | Flac of Flac.t
  | AACPlus of AACPlus.t
  | VoAacEnc of VoAacEnc.t
  | FdkAacEnc of FdkAacEnc.t
  | External of External.t
  | GStreamer of GStreamer.t

let kind_of_format = function
  | WAV w ->
      { Frame.audio = w.WAV.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | AVI a ->
      { Frame.audio = a.AVI.channels ;
        Frame.video = 1 ; Frame.midi = 0 }
  | MP3 m ->
      { Frame.audio = if m.MP3.stereo then 2 else 1 ;
        Frame.video = 0 ; Frame.midi = 0 }
  | Shine m ->
      { Frame.audio = m.Shine.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | Flac m ->
      { Frame.audio = m.Flac.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | AACPlus m ->
      { Frame.audio = m.AACPlus.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | VoAacEnc m ->
      { Frame.audio = m.VoAacEnc.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | FdkAacEnc m ->
      { Frame.audio = m.FdkAacEnc.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | Ogg l ->
      List.fold_left
        (fun k -> function
           | Ogg.Vorbis { Vorbis.channels = n; _} ->
               { k with Frame.audio = k.Frame.audio+n }
           | Ogg.Opus { Opus.channels = n; _} ->
               { k with Frame.audio = k.Frame.audio+n }
           | Ogg.Flac { Flac.channels = n; _} ->
               { k with Frame.audio = k.Frame.audio+n }
           | Ogg.Theora _ ->
               { k with Frame.video = k.Frame.video+1 }
           | Ogg.Dirac _ ->
               { k with Frame.video = k.Frame.video+1 }
           | Ogg.Speex { Speex.stereo = stereo; _} ->
               let n = if stereo then 2 else 1 in
               { k with Frame.audio = k.Frame.audio+n })
        { Frame.audio = 0 ; Frame.video = 0 ; Frame.midi = 0 }
        l
  | External e ->
      { Frame.audio = e.External.channels ;
        Frame.video = if e.External.video then 1 else 0 ; Frame.midi = 0 }
  | GStreamer e ->
    { Frame.audio = GStreamer.audio_channels e;
      Frame.video = GStreamer.video_channels e;
      Frame.midi = 0 }

let kind_of_format f =
  let k = kind_of_format f in
    { Frame.audio = Frame.mul_of_int k.Frame.audio ;
      Frame.video = Frame.mul_of_int k.Frame.video ;
      Frame.midi = Frame.mul_of_int k.Frame.midi }

let string_of_format = function
  | WAV w -> WAV.to_string w
  | AVI w -> AVI.to_string w
  | Ogg w -> Ogg.to_string w
  | MP3 w -> MP3.to_string w
  | Shine w -> Shine.to_string w
  | Flac w -> Flac.to_string w
  | AACPlus w -> AACPlus.to_string w
  | VoAacEnc w -> VoAacEnc.to_string w
  | FdkAacEnc w -> FdkAacEnc.to_string w
  | External w -> External.to_string w
  | GStreamer w -> GStreamer.to_string w

(** An encoder, once initialized, is something that consumes
  * frames, insert metadata and that you eventually close 
  * (triggers flushing). 
  * Insert metadata is really meant for inline metadata, i.e.
  * in most cases, stream sources. Otherwise, metadata are
  * passed when creating the encoder. For instance, the mp3 
  * encoder may accept metadata initally and write them as 
  * id3 tags but does not support inline metadata. 
  * Also, the ogg encoder supports inline metadata but restarts
  * its stream. This is ok, though, because the ogg container/streams 
  * is meant to be sequentialized but not the mp3 format. 
  * header contains data that should be sent first to streaming 
  * client. *)
type encoder = {
  insert_metadata : Meta.export_metadata -> unit ;
  (* Encoder are all called from the main 
   * thread so there's no need to protect this
   * value with a mutex so far.. *)
  mutable header : string option ;
  encode : Frame.t -> int -> int -> string ;
  stop : unit -> string
}

type factory = string -> Meta.export_metadata -> encoder

(** A plugin might or might not accept a given format.
  * If it accepts it, it gives a function creating suitable encoders. *)
type plugin = format -> factory option

let plug : plugin Plug.plug =
  Plug.create
    ~doc:"Methods to encode streams."
    ~insensitive:true
    "stream encoding formats"

exception Found of factory

(** Return the first available encoder factory for that format. *)
let get_factory fmt =
  try
    plug#iter
      (fun _ f ->
         match f fmt with
           | Some factory -> raise (Found factory)
           | None -> ()) ;
    raise Not_found
  with
    | Found factory -> factory
