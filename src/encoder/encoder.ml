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

  let to_string w = Printf.sprintf "WAV(%s)" (string_of_stereo w.stereo)

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
    skeleton   : bool ;
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
    Printf.sprintf "Vorbis(%s,%d channels,samplerate=%d,%s)"
      (string_of_mode v.mode)
      v.channels
      v.samplerate
      (if v.skeleton then "skeleton" else "no skeleton")

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
    Printf.sprintf "MP3(%s,quality=%d,samplerate=%d,bitrate=%d)"
      (string_of_stereo m.stereo)
      m.quality
      m.samplerate
      m.bitrate

end

module Theora =
struct

  type t = {
    quality : int ;
    width   : int ;
    height  : int
  }

  let to_string {quality=q} = Printf.sprintf "Theora(quality=%d)" q

end

module Ogg =
struct

  type item = Vorbis of Vorbis.t | Theora of Theora.t
  type t = item list

  let to_string l =
    Printf.sprintf "Ogg(%s)"
      (String.concat ","
         (List.map
            (function
               | Vorbis v -> Vorbis.to_string v
               | Theora t -> Theora.to_string t)
            l))

end

type format =
  | WAV of WAV.t
  | Ogg of Ogg.t
  | MP3 of MP3.t

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
               { k with Frame.video = k.Frame.video+1 })
        { Frame.audio = 0 ; Frame.video = 0 ; Frame.midi = 0 }
        l

let kind_of_format f =
  let k = kind_of_format f in
    { Frame.audio = Frame.mul_of_int k.Frame.audio ;
      Frame.video = Frame.mul_of_int k.Frame.video ;
      Frame.midi = Frame.mul_of_int k.Frame.midi }

let string_of_format = function
  | WAV w -> WAV.to_string w
  | Ogg w -> Ogg.to_string w
  | MP3 w -> MP3.to_string w

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
