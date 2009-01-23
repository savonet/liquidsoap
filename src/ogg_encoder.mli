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

 (** Ogg Stream Encoder *)

 (** {2 Types} *)

exception Invalid_data
exception Invalid_usage

(** Main type for the ogg decoder *)
type t

(** You may register new tracks on state Eos or Bos.
  * You can't register new track on state Streaming.
  * You may finalize at any state, provided at least
  * single track is registered. However, this is not
  * recommended. *)
type state = Eos | Streaming | Bos

(** Audio data type *)
type audio = float array array

(** Video data type *)
type video = RGB.t array array

(** A data unit *)
type 'a data = 
  { 
    data   : 'a;
    offset : int;
    length : int
  }

(** A track data is a data unit of either audio or video. *)
type track_data = 
  | Audio_data of audio data
  | Video_data of video data

(** A track encoder takes an encoder, the track data, 
  * the ogg logical stream, and fills the stream, and possibly
  * the encoder itself. *) 
type 'a track_encoder = t -> 'a data -> Ogg.Stream.t -> unit

(** Returns the first page of the stream,
  * to be placed at the very beginning. *)
type header_encoder = Ogg.Stream.t -> Ogg.Page.t

(** Returns an optional fisbone packet, which 
  * will contain the data for this stream to 
  * put in the ogg skeleton, if enabled in 
  * the encoder. *)
type fisbone_packet = Ogg.Stream.t -> Ogg.Stream.packet option

(** Returns the remaining header data, before data encoding starts. *)
type stream_start = Ogg.Stream.t -> string

(** Ends the track. *)
type end_of_stream = Ogg.Stream.t -> unit

(** A data encoder is an encoder for either a audio or a video track. *)
type data_encoder = 
  | Audio_encoder of audio track_encoder
  | Video_encoder of video track_encoder

(** The full stream encoder type. *)
type stream_encoder = 
  { 
    header_encoder : header_encoder;
    fisbone_packet : fisbone_packet;
    stream_start   : stream_start;
    data_encoder   : data_encoder;
    end_of_stream  : end_of_stream
  }

 (** {2 API} *)

 (** Usage: 
   *
   * Encoding: 
   * 
   * - [create ~skeleton name] : create a new decoder
   * - [register_track encoder stream_encoder] : register a new track
   * - ibid
   * - (...)
   * - [streams_start encoder] : start the tracks (optional)
   * - [encode encoder track_serial track_data] : encode data for one track
   * - ibid
   * - (...)
   * - [end_of_track encoder track_serial] : ends a track. (track end do not need to be simultaneous)
   * - (encode data for other tracks)
   * - [end_of_track encoder track_serial] (until all have been ended)
   * - [end_of_stream encoder]: ends all tracks at once (if not ended one by one previously) OR:
   * - [eos encoder]: set state to eos (needs that all tracks have been ended before)
   * - [register_track encoder stream_encoder] : register a new track, starts a new sequentialized stream
   * - And so on.. 
   *
   * You get encoded data by calling [get_data], [peek_data] or [flush].
   *
   * See: http://xiph.org/ogg/doc/oggstream.html for more details on the 
   * specifications of an ogg stream. This API reflects exactly what is recomended to do. *)

(** Create a new encoder. Add an ogg skeleton if [skeleton] is [true]. *)
val create : skeleton:bool -> string -> t 

(** Get the state of an encoder. *)
val state : t -> state

(** Get and remove encoded data.. *)
val get_data : t -> string

(** Peek encoded data without removing it. *)
val peek_data : t -> string

(** Flush data from all tracks in the stream. *)
val flush : t -> string

(** Add an ogg page. *)
val add_page : t -> Ogg.Page.t -> unit

(** Register a new track to the stream.
  * The state needs to be [Bos] or [Eos]. 
  * Returns the serial number of the registered ogg 
  * stream. *)
val register_track : t -> stream_encoder -> nativeint

(** Start streams, set state to [Streaming]. *)
val streams_start : t -> unit

(** Encode data. Implicitely calls [streams_start]
  * if not called before. Fails if state is not [Streaming] *)
val encode : t -> nativeint -> track_data -> unit

(** Finish a track. Also flushes track data. *)
val end_of_track : t -> nativeint -> unit

(** Set state to [Eos]. Fails if some track have not been
  * ended with [end_of_track]. *)
val eos : t -> unit

(** End all tracks in the stream, set state to [Eos]. *)
val end_of_stream : t -> unit

