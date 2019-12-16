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

  (** Length of the generator in ticks. *)
  val length : t -> int

  (** Duration of data (in ticks) before the next break, -1 if there's none. *)
  val remaining : t -> int

  (** Clear the generator. *)
  val clear : t -> unit

  (** Fill a frame from the generator. *)
  val fill : t -> Frame.t -> unit

  (** Forget a given duration (in ticks) of the generator. *)
  val remove : t -> int -> unit

  (** Add metadata. *)
  val add_metadata : t -> Frame.metadata -> unit
end

(** Content-agnostic generator. *)
module Generator : sig
  (** A generator. *)
  type 'a t

  (** Create a generator. *)
  val create : unit -> 'a t

  (** Empty a generator. *)
  val clear : 'a t -> unit

  (** Length of data contained in a generator (in ticks). *)
  val length : 'a t -> int

  (** Remove given amount (in ticks) of data. *)
  val remove : 'a t -> int -> unit

  (** [put data ofs len] adds [len] of data [data] starting from [ofs]. Data is
      not copied. *)
  val put : 'a t -> 'a -> int -> int -> unit

  (** Get given amount of data from a generator. Returns a list where each
      element will typically be passed to a blit: its elements are of the form
      [b,o,o',l] where [o] is the offset of data in the block [b], [o'] is the
      position at which it should be written (the first position [o'] will
      always be [0]), and [l] is the length of data to be taken from that
      block. *)
  val get : 'a t -> int -> ('a * int * int * int) list
end

(** A generator for metadata. *)
module Metadata : sig
  (** A metadata generator. *)
  type t

  (** Create a generator. *)
  val create : unit -> t

  (** Clear generator. *)
  val clear : t -> unit

  (** Length in ticks. *)
  val length : t -> int

  (** Time until next break, or -1 if there is none. *)
  val remaining : t -> int

  (** Drop a portion at the begining. *)
  val advance : t -> int -> unit

  (** Drop break at the beginning. This should be called after filling a partial
      frame manually (i.e. not using [fill]). *)
  val drop_initial_break : t -> unit

  (** Retrieve all metadata between now and given time. *)
  val metadata : t -> int -> (int * Frame.metadata) list

  (** Feed all breaks and metadata from a frame. *)
  val feed_from_frame : t -> Frame.t -> unit

  (** Fill a frame (until the next break) with metadata and add a break at the
      end. *)
  val fill : t -> Frame.t -> unit
end

(** A generator that consumes frames (or frame content) and produces frames. *)
module From_frames : sig
  (** A generator. *)
  type t

  (** Create a generator. *)
  val create : unit -> t

  (** Remove all data from a generator. *)
  val clear : t -> unit

  (** Length of data in the generator. *)
  val length : t -> int

  (** Remaining data before next break (-1) if there is none. *)
  val remaining : t -> int

  (** Add metadata at the current position. *)
  val add_metadata : t -> Frame.metadata -> unit

  (** Add a break at the current position. *)
  val add_break : t -> unit

  (** Remove data. *)
  val remove : t -> int -> unit

  (** Feed the generator with data. *)
  val feed :
    t ->
    ?copy:bool ->
    ?breaks:int list ->
    ?metadata:(int * Frame.metadata) list ->
    Frame.content ->
    int ->
    int ->
    unit

  (** Feed the generator with the contents of a frame (the contents is
      copied). *)
  val feed_from_frame : t -> Frame.t -> unit

  (** Fill a frame from the generator. *)
  val fill : t -> Frame.t -> unit
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

  (** Create a generator with given mode. *)
  val create : mode -> t

  (** Current mode: in Audio mode (resp. Video), only audio (resp. Audio) can be
      fed, otherwise both have to be fed. *)
  val mode : t -> mode

  (** Change the generator mode. Only allowed when there is as much audio as
      video. *)
  val set_mode : t -> mode -> unit

  (** Length of available audio data. *)
  val audio_length : t -> int

  (** Length of available video data. *)
  val video_length : t -> int

  (** Length of data available in both audio and video. *)
  val length : t -> int

  (** Size of audio data in bytes. *)
  val audio_size : t -> int

  (** Size of video data in bytes. *)
  val video_size : t -> int

  (** Size of data in bytes. *)
  val size : t -> int

  (** Duration of data (in ticks) before the next break, -1 if there's none. *)
  val remaining : t -> int

  (** Add metadata at the minimum position of audio and video. You probably want
      to call this when there is as much audio as video. *)
  val add_metadata : t -> Frame.metadata -> unit

  (** Add a track limit. Audio and video length should be equal. *)
  val add_break : ?sync:[ `Strict | `Ignore | `Drop ] -> t -> unit

  (* [put_audio buffer data offset length]: offset and length
   * are in samples ! *)
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit

  (* [put_video buffer data offset length]: offset and length
   * are in samples ! *)
  val put_video : t -> Frame.video_t array -> int -> int -> unit

  (** Feed from a frame, only copying data according to the mode. *)
  val feed_from_frame : t -> Frame.t -> unit

  (** Fill a frame from the generator. *)
  val fill : t -> Frame.t -> unit

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
