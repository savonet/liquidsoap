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

let string_of_stereo s =
  if s then "stereo" else "mono"

module WAV =
struct

  type t = { stereo : bool }

  let to_string w = Printf.sprintf "%%wav(%s)" (string_of_stereo w.stereo)

end

module Vorbis =
struct

  type quality = float
  type bitrate = int
  type mode =
    | VBR of quality                 (* Variable bitrate. *)
    | CBR of bitrate                 (* Constant bitrate. *)
    | ABR of bitrate*bitrate*bitrate (* Average: min,avg,max. *)

  type t = {
    channels   : int ;
    mode       : mode ;
    samplerate : int ;
  }

  let string_of_mode = function
    | ABR (min,avg,max) ->
        Printf.sprintf "ABR(%d,%d,%d)" min avg max
    | CBR bitrate ->
        Printf.sprintf "CBR(%d)" bitrate
    | VBR q ->
        Printf.sprintf "quality=%.2f" q

  let to_string v =
    Printf.sprintf "%%vorbis(%s,%d channels,samplerate=%d)"
      (string_of_mode v.mode)
      v.channels
      v.samplerate
end

module MP3 =
struct

  type t = {
    stereo     : bool ;
    quality    : int ;
    samplerate : int ;
    bitrate    : int ;
  }

  let to_string m =
    Printf.sprintf "%%mp3(%s,quality=%d,samplerate=%d,bitrate=%d)"
      (string_of_stereo m.stereo)
      m.quality
      m.samplerate
      m.bitrate

end

module External =
struct

  exception No_process

  type restart_condition = Delay of int | Track | No_condition

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
        | Delay d      -> Printf.sprintf "restart_after_delay=%i" d
        | Track        -> "restart_on_new_track"
        | No_condition -> ""
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
    aspect_denominator : int
  }

  let bit_ctl_to_string bit_ctl = 
    match bit_ctl with
      | Quality x -> Printf.sprintf "quality=%d" x
      | Bitrate x -> Printf.sprintf "bitrate=%d" x

  let to_string th = 
    let f = Lazy.force in
    Printf.sprintf "Theora(%s,width=%d,height=%d,picture_width=%d,\
                           picture_height=%d,picture_x=%d,picture_y=%d,\
                           aspect_numerator=%d,aspect_denominator=%d)" 
    (bit_ctl_to_string th.bitrate_control) (f th.width) (f th.height)
    (f th.picture_width) (f th.picture_height) th.picture_x th.picture_y
    th.aspect_numerator th.aspect_denominator

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

  type item = Speex of Speex.t | Vorbis of Vorbis.t | Theora of Theora.t | Dirac of Dirac.t
  type t = item list

  let to_string l =
    Printf.sprintf "%%ogg(%s)"
      (String.concat ","
         (List.map
            (function
               | Vorbis v -> Vorbis.to_string v
               | Theora t -> Theora.to_string t
               | Speex  s -> Speex.to_string s
               | Dirac  d -> Dirac.to_string d)
            l))

end

type format =
  | WAV of WAV.t
  | Ogg of Ogg.t
  | MP3 of MP3.t
  | External of External.t

let kind_of_format = function
  | WAV w ->
      { Frame.audio = if w.WAV.stereo then 2 else 1 ;
        Frame.video = 0 ; Frame.midi = 0 }
  | MP3 m ->
      { Frame.audio = if m.MP3.stereo then 2 else 1 ;
        Frame.video = 0 ; Frame.midi = 0 }
  | Ogg l ->
      List.fold_left
        (fun k -> function
           | Ogg.Vorbis { Vorbis.channels = n } ->
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
  | External w -> External.to_string w

(** An encoder, once initialized, is something that consumes
  * frames, and that you eventually close (triggers flushing). *)
type encoder = {
  reset : Frame.metadata -> string ;
  encode : Frame.t -> int -> int -> string ;
  stop : unit -> string
}

type factory = string -> encoder

(** A plugin might or might not accept a given format.
  * If it accepts it, it gives a function creating suitable encoders. *)
type plugin = format -> factory option

let plug : plugin Plug.plug =
  Plug.create
    ~doc:"Methods to encode streams."
    ~insensitive:true
    "stream encoding formats"

exception Found of (string -> encoder)

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
