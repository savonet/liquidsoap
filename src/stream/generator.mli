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

(** Operations on generators, which are like FIFO for multimedia data. They are
    efficiently handling small chunks of data (as found in frames) without copy,
    and also metadata. *)

(** Raised when trying to feed a generator with data of incorrect type (wrong
    number of audio channels, etc.). *)
exception Incorrect_stream_type

(** Signature for a generator. *)
module type S = sig
  type t

  val length : t -> int
  (** Length of the generator in ticks. *)

  val remaining : t -> int
  (** Duration of data (in ticks) before the next break, -1 if there's none. *)

  val clear : t -> unit
  (** Clear the generator. *)

  val fill : t -> Frame.t -> unit
  (** Fill a frame from the generator. *)

  val remove : t -> int -> unit
  (** Forget a given duration (in ticks) of the generator. *)

  val add_metadata : t -> Frame.metadata -> unit
  (** Add metadata. *)
end

(** Content-agnostic generator. *)
module Generator : sig
  (** A generator. *)
  type 'a t

  val create : unit -> 'a t
  (** Create a generator. *)

  val clear : 'a t -> unit
  (** Empty a generator. *)

  val length : 'a t -> int
  (** Length of data contained in a generator (in ticks). *)

  val remove : 'a t -> int -> unit
  (** Remove given amount (in ticks) of data. *)

  val put : 'a t -> 'a -> int -> int -> unit
  (** [put data ofs len] adds [len] of data [data] starting from [ofs]. Data is
      not copied. *)

  val get : 'a t -> int -> ('a * int * int * int) list
  (** Get given amount of data from a generator. Returns a list where each
      element will typically be passed to a blit: its elements are of the form
      [b,o,o',l] where [o] is the offset of data in the block [b], [o'] is the
      position at which it should be written (the first position [o'] will
      always be [0]), and [l] is the length of data to be taken from that
      block. *)
end

(** A generator for metadata. *)
module Metadata : sig
  (** A metadata generator. *)
  type t

  val create : unit -> t
  (** Create a generator. *)

  val clear : t -> unit
  (** Clear generator. *)

  val length : t -> int
  (** Length in ticks. *)

  val remaining : t -> int
  (** Time until next break, or -1 if there is none. *)

  val advance : t -> int -> unit
  (** Drop a portion at the begining. *)

  val drop_initial_break : t -> unit
  (** Drop break at the beginning. This should be called after filling a partial
      frame manually (i.e. not using [fill]). *)

  val metadata : t -> int -> (int * Frame.metadata) list
  (** Retrieve all metadata between now and given time. *)

  val feed_from_frame : t -> Frame.t -> unit
  (** Feed all breaks and metadata from a frame. *)

  val fill : t -> Frame.t -> unit
  (** Fill a frame (until the next break) with metadata and add a break at the
      end. *)
end

(** A generator that consumes frames (or frame content) and produces frames. *)
module From_frames : sig
  (** A generator. *)
  type t

  val create : unit -> t
  (** Create a generator. *)

  val clear : t -> unit
  (** Remove all data from a generator. *)

  val length : t -> int
  (** Length of data in the generator. *)

  val remaining : t -> int
  (** Remaining data before next break (-1) if there is none. *)

  val add_metadata : t -> Frame.metadata -> unit
  (** Add metadata at the current position. *)

  val add_break : t -> unit
  (** Add a break at the current position. *)

  val remove : t -> int -> unit
  (** Remove data. *)

  val feed :
    t ->
    ?copy:bool ->
    ?breaks:int list ->
    ?metadata:(int * Frame.metadata) list ->
    Frame.content ->
    int ->
    int ->
    unit
  (** Feed the generator with data. *)

  val feed_from_frame : t -> Frame.t -> unit
  (** Feed the generator with the contents of a frame (the contents is
      copied). *)

  val fill : t -> Frame.t -> unit
  (** Fill a frame from the generator. *)
end

(** Generator that consumes audio and video asynchronously, and produces
    frames. *)
module From_audio_video : sig
  type t

  (** In [`Audio] mode, only audio can be put in the buffer, and similarly for
      the [`Video] mode. In [`Both] mode, both types of content can be fed into
      the generator, asynchronously, and they exit the buffer synchronously.
      [`Undefined] forbids any feeding, it's useful to make sure a meaningful
      mode is assigned before any use. *)
  type mode = [ `Audio | `Video | `Both | `Undefined ]

  val create : mode -> t
  (** Create a generator with given mode. *)

  val mode : t -> mode
  (** Current mode: in Audio mode (resp. Video), only audio (resp. Audio) can be
      fed, otherwise both have to be fed. *)

  val set_mode : t -> mode -> unit
  (** Change the generator mode. Only allowed when there is as much audio as
      video. *)

  val audio_length : t -> int
  (** Length of available audio data. *)

  val video_length : t -> int
  (** Length of available video data. *)

  val length : t -> int
  (** Length of data available in both audio and video. *)

  val audio_size : t -> int
  (** Size of audio data in bytes. *)

  val video_size : t -> int
  (** Size of video data in bytes. *)

  val size : t -> int
  (** Size of data in bytes. *)

  val remaining : t -> int
  (** Duration of data (in ticks) before the next break, -1 if there's none. *)

  val add_metadata : t -> Frame.metadata -> unit
  (** Add metadata at the minimum position of audio and video. You probably want
      to call this when there is as much audio as video. *)

  val add_break : ?sync:[ `Strict | `Ignore | `Drop ] -> t -> unit
  (** Add a track limit. Audio and video length should be equal. *)

  (* [put_audio buffer data offset length]: offset and length
   * are in samples ! *)
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit

  (* [put_video buffer data offset length]: offset and length
   * are in samples ! *)
  val put_video : t -> Frame.video_t array -> int -> int -> unit

  val feed_from_frame : t -> Frame.t -> unit
  (** Feed from a frame, only copying data according to the mode. *)

  val fill : t -> Frame.t -> unit
  (** Fill a frame from the generator. *)

  val remove : t -> int -> unit
  val clear : t -> unit
end

(** Generator not only with Output but also with ASynchronous Input. *)
module type S_Asio = sig
  type t

  val length : t -> int (* ticks *)

  val audio_length : t -> int
  val video_length : t -> int
  val remaining : t -> int (* ticks *)

  val clear : t -> unit
  val fill : t -> Frame.t -> unit
  val add_metadata : t -> Frame.metadata -> unit
  val add_break : ?sync:[ `Strict | `Ignore | `Drop ] -> t -> unit
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit
  val put_video : t -> Frame.video_t array -> int -> int -> unit
  val set_mode : t -> [ `Audio | `Video | `Both | `Undefined ] -> unit
end

(** Same as [From_audio_video] but with two extra features useful for streaming
    decoders: it is thread safe and supports overfull buffer management. *)
module From_audio_video_plus : sig
  type t

  (** Same as [From_audio_video]. *)
  type mode = [ `Audio | `Video | `Both | `Undefined ]

  (** How to handle overfull buffers:
    * drop old data, keeping at most [len] ticks. *)
  type overfull = [ `Drop_old of int ]

  val create :
    ?lock:Mutex.t ->
    ?overfull:overfull ->
    kind:Frame.content_kind ->
    log:(string -> unit) ->
    log_overfull:bool ->
    mode ->
    t

  val mode : t -> From_audio_video.mode
  val set_mode : t -> From_audio_video.mode -> unit
  val audio_length : t -> int
  val video_length : t -> int
  val length : t -> int
  val remaining : t -> int
  val audio_size : t -> int
  val video_size : t -> int
  val size : t -> int
  val set_rewrite_metadata : t -> (Frame.metadata -> Frame.metadata) -> unit
  val add_metadata : t -> Frame.metadata -> unit
  val add_break : ?sync:[ `Strict | `Ignore | `Drop ] -> t -> unit

  (* [put_audio buffer data offset length]:
   * offset and length are in audio samples! *)
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit

  (* [put_video buffer data offset length]:
   * offset and length are in video samples! *)
  val put_video : t -> Frame.video_t array -> int -> int -> unit
  val fill : t -> Frame.t -> unit
  val remove : t -> int -> unit
  val clear : t -> unit
end
