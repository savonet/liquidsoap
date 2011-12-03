(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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
  end = 
  struct
    type export_metadata = Frame.metadata
    let export_metadata m =
      let ret = Hashtbl.create 10 in
      let l = conf_export_metadata#get in
      Hashtbl.iter (fun x y -> if List.mem (String.lowercase x) l then
                               Hashtbl.add ret x y) m;
    ret
    let to_metadata m = m
    let empty_metadata = Hashtbl.create 0
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
  }

  let to_string m =
    Printf.sprintf
      "%%flac(channels=%i,bits_per_sample=%i,samplerate=%d,compression=%i)"
      m.channels m.bits_per_sample m.samplerate m.compression

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
    Printf.sprintf "%%external(channels=%i,samplerate=%i,header=%s,\
                              restart_on_crash=%s,%s,process=%s)"
      e.channels
      e.samplerate
      (string_of_bool e.header)
      (string_of_bool e.restart_on_crash)
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
    complexity        : int option
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
      "%%speex(%s,%s,samplerate=%d,mode=%s,frames_per_packet=%d%s)"
      (string_of_stereo m.stereo)
      (string_of_br_ctl m.bitrate_control)
      m.samplerate
      (string_of_mode m.mode)
      m.frames_per_packet
      (string_of_complexity m.complexity)

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
    aspect_denominator : int
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
               | Dirac  d -> Dirac.to_string d)
            l))

end

type format =
  | WAV of WAV.t
  | Ogg of Ogg.t
  | MP3 of MP3.t
  | Flac of Flac.t
  | AACPlus of AACPlus.t
  | VoAacEnc of VoAacEnc.t
  | External of External.t

let kind_of_format = function
  | WAV w ->
      { Frame.audio = w.WAV.channels ;
        Frame.video = 0 ; Frame.midi = 0 }
  | MP3 m ->
      { Frame.audio = if m.MP3.stereo then 2 else 1 ;
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
  | Ogg l ->
      List.fold_left
        (fun k -> function
           | Ogg.Vorbis { Vorbis.channels = n } ->
               { k with Frame.audio = k.Frame.audio+n }
           | Ogg.Flac { Flac.channels = n } ->
               { k with Frame.audio = k.Frame.audio+n }
           | Ogg.Theora _ ->
               { k with Frame.video = k.Frame.video+1 }
           | Ogg.Dirac _ ->
               { k with Frame.video = k.Frame.video+1 }
           | Ogg.Speex { Speex.stereo = stereo } ->
               let n = if stereo then 2 else 1 in
               { k with Frame.audio = k.Frame.audio+n })
        { Frame.audio = 0 ; Frame.video = 0 ; Frame.midi = 0 }
        l
  | External e ->
      { Frame.audio = e.External.channels ;
        Frame.video = 0 ; Frame.midi = 0 }

let kind_of_format f =
  let k = kind_of_format f in
    { Frame.audio = Frame.mul_of_int k.Frame.audio ;
      Frame.video = Frame.mul_of_int k.Frame.video ;
      Frame.midi = Frame.mul_of_int k.Frame.midi }

let string_of_format = function
  | WAV w -> WAV.to_string w
  | Ogg w -> Ogg.to_string w
  | MP3 w -> MP3.to_string w
  | Flac w -> Flac.to_string w
  | AACPlus w -> AACPlus.to_string w
  | VoAacEnc w -> VoAacEnc.to_string w
  | External w -> External.to_string w

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
