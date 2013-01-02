(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

(** Raised when trying to feed a generator with data of incorrect type (wrong
    number of audio channels, etc.). *)
exception Incorrect_stream_type

module type S =
sig
  type t

  (** Length of the generator in ticks. *)
  val length : t -> int

  (** Duration of data (in ticks) before the next break, -1 if there's none. *)
  val remaining : t -> int

  (** Clear the generator. *)
  val clear : t -> unit

  (** Append a fram to the generator. *)
  val fill : t -> Frame.t -> unit

  (** Forget a given duration (in ticks) of the generator. *)
  val remove : t -> int -> unit

  (** Add metadata. *)
  val add_metadata : t -> Frame.metadata -> unit
end

module Generator :
sig
  type 'a t
  val create : unit -> 'a t
  val clear : 'a t -> unit
  val length : 'a t -> int
  val remove : 'a t -> int -> unit
  val put : 'a t -> 'a -> int -> int -> unit
  val get : 'a t -> int -> ('a * int * int * int) list
end

(** A generator that consumes frames (or frame content) and produces frames. *)
module From_frames :
sig
  type t
  val create : unit -> t
  val clear : t -> unit
  val length : t -> int
  val remaining : t -> int
  val add_metadata : t -> Frame.metadata -> unit
  val add_break : t -> unit
  val remove : t -> int -> unit
  val feed : t ->
    ?copy:bool ->
    ?breaks:(int list) -> ?metadata:((int*Frame.metadata) list) ->
    Frame.content -> int -> int -> unit
  val feed_from_frame : t -> Frame.t -> unit
  val fill : t -> Frame.t -> unit
end

(** Generator that consumes audio and video asynchronously, and produces
    frames. *)
module From_audio_video :
sig
  type t

  (** In [`Audio] mode, only audio can be put in the buffer, and similarly for
      the [`Video] mode. In [`Both] mode, both types of content can be fed into
      the generator, asynchronously, and they exit the buffer synchronously.
      [`Undefined] forbids any feeding, it's useful to make sure a meaningful
      mode is assigned before any use. *)
  type mode = [ `Audio | `Video | `Both | `Undefined ]

  val create : mode -> t

  val mode : t -> mode
  val set_mode : t -> mode -> unit

  val audio_length : t -> int
  val video_length : t -> int
  val length : t -> int
  val remaining : t -> int

  val add_metadata : t -> Frame.metadata -> unit
  val add_break : ?sync:[`Strict|`Ignore|`Drop] -> t -> unit

  (* [put_audio buffer data offset length]: offset and length
   * are in samples ! *)
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit
  (* [put_video buffer data offset length]: offset and length
   * are in samples ! *)
  val put_video : t -> Frame.video_t array -> int -> int -> unit
  val fill : t -> Frame.t -> unit

  val remove : t -> int -> unit
  val clear : t -> unit
end

(** Generator not only with Output but also with ASynchronous Input. *)
module type S_Asio =
sig
  type t
  val length : t -> int (* ticks *)
  val audio_length : t -> int
  val video_length : t -> int
  val remaining : t -> int (* ticks *)
  val clear : t -> unit
  val fill : t -> Frame.t -> unit
  val add_metadata : t -> Frame.metadata -> unit
  val add_break : ?sync:[`Strict|`Ignore|`Drop] -> t -> unit
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit
  val put_video : t -> Frame.video_t array -> int -> int -> unit
  val set_mode : t -> [ `Audio | `Video | `Both | `Undefined ] -> unit
end

(** Same as From_audio_video but with two extra features useful for streaming
    decoders: it is thread safe and supports overfull buffer management. *)
module From_audio_video_plus :
sig
  type t

  (** Same as [From_audio_video]. *)
  type mode = [ `Audio | `Video | `Both | `Undefined ]

  (** How to handle overfull buffers:
    * drop old data, keeping at most [len] ticks. *)
  type overfull = [ `Drop_old of int ]

  val create : ?lock:Mutex.t -> ?overfull:overfull ->
               kind:Frame.content_kind ->
               log:(string -> unit) -> mode -> t

  val mode : t -> From_audio_video.mode
  val set_mode : t -> From_audio_video.mode -> unit

  val audio_length : t -> int
  val video_length : t -> int
  val length : t -> int
  val remaining : t -> int

  val set_rewrite_metadata : t -> (Frame.metadata -> Frame.metadata) -> unit
  val add_metadata : t -> Frame.metadata -> unit
  val add_break : ?sync:[`Strict|`Ignore|`Drop] -> t -> unit

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
