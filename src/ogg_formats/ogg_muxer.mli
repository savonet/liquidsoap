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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Ogg Stream Encoder *)

val log : Log.t

(** {2 Types} *)

exception Invalid_data

exception Invalid_usage

(** Audio data type *)
type audio = float array array

(** Video data type *)
type video = Video.buffer

(** A data unit *)
type 'a data = {data: 'a; offset: int; length: int}

(** A track data is a data unit of either audio or video. *)
type track_data = Audio_data of audio data | Video_data of video data

(** A track encoder takes the track data,
  * the ogg logical stream, and fills the stream.
  * If the encoding process outputs ogg pages, then
  * the encoder should use the last argument to add its pages
  * to the stream. *)
type 'a track_encoder =
  'a data -> Ogg.Stream.stream -> (Ogg.Page.t -> unit) -> unit

(** Returns the first page of the stream,
  * to be placed at the very beginning. *)
type header_encoder = Ogg.Stream.stream -> Ogg.Page.t

(** Return the end time of a page, in milliseconds. *)
type position = Unknown | Time of float

(** Type for a function returning a page's ending time. *)
type page_end_time = Ogg.Page.t -> position

(** Returns an optional fisbone packet, which
  * will contain the data for this stream to
  * put in the ogg skeleton, if enabled in
  * the encoder. *)
type fisbone_packet = Ogg.Stream.stream -> Ogg.Stream.packet option

(** Returns the remaining header data, before data encoding starts. *)
type stream_start = Ogg.Stream.stream -> Ogg.Page.t list

(** Ends the track. *)
type end_of_stream = Ogg.Stream.stream -> unit

(** A data encoder is an encoder for either a audio or a video track. *)
type data_encoder =
  | Audio_encoder of audio track_encoder
  | Video_encoder of video track_encoder

(** The full stream encoder type. *)
type stream_encoder = {
  header_encoder: header_encoder;
  fisbone_packet: fisbone_packet;
  stream_start: stream_start;
  data_encoder: data_encoder;
  end_of_page: page_end_time;
  end_of_stream: end_of_stream;
}

(** Main type for the ogg encoder *)
type t

(** You may register new tracks on state Eos or Bos.
  * You can't register new track on state Streaming. *)
type state = Eos | Streaming | Bos

(** {2 API} *)

(** Usage:
   *
   * Encoding:
   *
   * - [create ~skeleton name] : create a new encoder
   * - [register_track encoder stream_encoder] : register a new track
   * - ibid
   * - (...)
   * - [streams_start encoder] : start the tracks (optional)
   * - [encode encoder track_serial track_data] : encode data for one track
   * - ibid
   * - (...)
   * - (encode data for other tracks)
   * - [end_of_track encoder track_serial] : ends a track. (track end do not need to be simultaneous)
   * - (...)
   * - [end_of_stream encoder]: ends all tracks as well as the encoder. Set Eos state on the encoder.
   * - [register_track encoder stream_encoder] : register a new track, starts a new sequentialized stream
   * - And so on..
   *
   * You get encoded data by calling [get_data], [peek_data].
   *
   * See: http://xiph.org/ogg/doc/oggstream.html for more details on the
   * specifications of an ogg stream. This API reflects exactly what is recomended to do. *)

(** Create a new encoder.
  * Add an ogg skeleton if [skeleton] is [true]. *)
val create : skeleton:bool -> string -> t

(** Get the state of an encoder. *)
val state : t -> state

(** Get and remove encoded data.. *)
val get_data : t -> Strings.t

(** Get header of a stream. *)
val get_header : t -> Strings.t

(** Peek encoded data without removing it. *)
val peek_data : t -> Strings.t

(** Register a new track to the stream.
  * The state needs to be [Bos] or [Eos].
  *
  * [fill] parameter is used to try to control ogg
  * logical page's size. See [Ogg.get_page] for more
  * details.
  *
  * Returns the serial number of the registered ogg
  * stream. *)
val register_track : ?fill:int -> t -> stream_encoder -> nativeint

(** Start streams, set state to [Streaming]. *)
val streams_start : t -> unit

(** Encode data. Implicitely calls [streams_start]
  * if not called before. Fails if state is not [Streaming] *)
val encode : t -> nativeint -> track_data -> unit

(** Finish a track. Raises [Not_found] if
  * no such track exists. *)
val end_of_track : t -> nativeint -> unit

(** Ends all tracks, flush remaining encoded data.
  * Set state to [Eos]. *)
val end_of_stream : t -> unit

(** {2 Utils} *)

(** flush all availables pages from an ogg stream *)
val flush_pages : Ogg.Stream.stream -> Ogg.Page.t list
