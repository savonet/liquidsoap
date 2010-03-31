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
(** Only supported for now: plannar YUV formats. *)
type video_format = 
   | Yuvj_420   (* Planar YCbCr 4:2:0. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_422   (* Planar YCbCr 4:2:2. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_444   (* Planar YCbCr 4:4:4. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
type video =
 {
    format    : video_format;
    fps       : float; (** Video frames per second *)
    width   : int; (** Width of the Y' luminance plane *)
    height  : int; (** Height of the luminance plane *)
    y_stride  : int; (** Length, in bytes, per line *)
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

(** Remove all tracks of the given type. *)
val drop_track : track -> t -> unit

(** Feed new pages into the decoder 
  *
  * Raises [End_of_stream] is the stream has ended.
  * In this case, you can try [reset] to see if there is a 
  * new sequentialized stream. 
  *
  * Raises [Ogg.Not_enough_data] if no data could be read
  * from the source. This can happen with incomplete streams. *)
val feed : t -> unit

(** Decode audio data, if possible. 
  * Decoded data is passed to the second argument. 
  *
  * Raises [Ogg.Not_enough_data] if more data could be added 
  * you should call [feed] in this case. 
  *
  * Raises [End_of_stream] is the stream has ended.
  * In this case, you can try [reset] to see if there is a
  * new sequentialized stream. *)
val decode_audio : t -> (audio * Frame.metadata option -> unit) -> unit

(** Decode audio data, if possible.
  * Decoded data is passed to the second argument.
  * This function implicitely calls [feed] if not enough data 
  * are available.
  * 
  * Raises [End_of_stream] is the stream has ended.
  * In this case, you can try [reset] to see if there is a
  * new sequentialized stream. *)
val decode_audio_rec : t -> (audio * Frame.metadata option -> unit) -> unit

(** Decode video data, if possible. 
  * Decoded data is passed to the second argument. 
  * Raises [Ogg.Not_enough_data] if more data could be added
  * you should call [feed] in this case.
  *
  * Raises [End_of_stream] is the stream has ended.
  * In this case, you can try [reset] to see if there is a
  * new sequentialized stream. *)
val decode_video : t -> (video * Frame.metadata option -> unit) -> unit 

(** Decode video data, if possible.
  * Decoded data is passed to the second argument. 
  * This function implicitely calls [feed] if not enough data
  * are available. 
  * 
  * Raises [End_of_stream] is the stream has ended.
  * In this case, you can try [reset] to see if there is a
  * new sequentialized stream. *)
val decode_video_rec : t -> (video * Frame.metadata option -> unit) -> unit
