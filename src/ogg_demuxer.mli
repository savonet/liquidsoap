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

 (** Ogg stream demuxer *)

type metadata = string*((string*string) list)
type 'a decoder = ('a*(metadata option) -> unit) -> unit
type audio = (float array array)*int
type video_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type video =
 {
    fps       : float; (** Video frames per second *)
    y_width   : int; (** Width of the Y' luminance plane *)
    y_height  : int; (** Height of the luminance plane *)
    y_stride  : int; (** Length, in bytes, per line *)
    uv_width  : int; (** Width of the Cb and Cr chroma planes *)
    uv_height : int; (** Height of the chroma planes *)
    uv_stride : int; (** Length, in bytes, per line *)
    y : video_data; (** luminance data *)
    u : video_data; (** Cb data *)
    v : video_data; (** Cr data *)
 }
type decoders =
    | Video of video decoder
    | Audio of audio decoder
    | Unknown

type t

type track = Audio_track | Video_track

exception Invalid_stream
exception End_of_stream

val ogg_decoders : ((Ogg.Stream.packet -> bool)*
                    (Ogg.Stream.t -> decoders)) Plug.plug

(** Initiate a decoder with the given ogg sync structure. *)
val init : Ogg.Sync.t -> t 

(** [true] if the decoder reached the end of all streams. *)
val eos : t -> bool

(** Reset encoder, try to parse a new sequentialized stream.
  * To use when end_of_stream has been reached. *)
val reset : t -> unit

(** [true] if the decoder has a track of that type. *)
val has_track : track -> t -> bool 

(** Decode audio data, if possible. 
  * Decoded data is passed to the second argument. *)
val decode_audio : t -> (audio * Frame.metadata option -> unit) -> unit

(** Decode video data, if possible. 
  * Decoded data is passed to the second argument. *)
val decode_video : t -> (video * Frame.metadata option -> unit) -> unit 
