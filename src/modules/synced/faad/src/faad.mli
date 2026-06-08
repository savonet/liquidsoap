(*
 * Copyright (C) 2003-2008 Samuel Mimram
 *           (C) 2006-2010 The Savonet Team
 *
 * Ocaml-faad is free software; you can redistribute it and/or modify<F12>
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-faad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-faad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Library for decoding AAC and AAC+ streams.
  *
  * @author Samuel Mimram
  *)

(** Internal state of a decoder. *)
type t

(** An error occurred... *)
exception Error of int

exception Failed

(** Get the error message corresponding to a raised [Error]. *)
val error_message : int -> string

(** A decode call can eat up to [min_bytes_per_channel] bytes per decoded
    channel, so at least so much bytes per channel should be available in this
    stream. *)
val min_bytes_per_channel : int

val create : unit -> t

(** [init dec buf ofs len] initializes a decoder given the [len] bytes of data *
    in [buf] starting at offset [ofs]. It returns the offset (number of bytes to
    * skip), the samplerate and the number of channels of the stream. This
    function * should be used for AAC data. *)
val init : t -> Bytes.t -> int -> int -> int * int * int

(** [decode dec buf ofs len] decodes at most [len] bytes of data in [buf] *
    starting at offset [ofs]. It returns the number of bytes actually decoded *
    and the decoded data (non-interleaved). *)
val decode : t -> Bytes.t -> int -> int -> int * float array array

val post_sync_reset : t -> unit

(** Heuristic guess of the offset of the beginning of a frame. *)
val find_frame : string -> int

module Mp4 : sig
  type decoder = t

  (** An MP4 reader. *)
  type t

  (** A track number. *)
  type track = int

  (** A sample number. *)
  type sample = int

  (** Data read. Same signature as [Unix.read]. *)
  type read = bytes -> int -> int -> int

  (** Detect whether the file is an MP4 given at least 8 bytes of its header. *)
  val is_mp4 : string -> bool

  (** Open an MP4 file. *)
  val openfile :
    ?write:(Bytes.t -> int) ->
    ?seek:(int -> int) ->
    ?trunc:(unit -> int) ->
    read ->
    t

  val openfile_fd : Unix.file_descr -> t

  (** Total number of tracks. *)
  val tracks : t -> int

  (** Find the first AAC track. *)
  val find_aac_track : t -> track

  (** Initialize a decoder. *)
  val init : t -> decoder -> track -> int * int

  (** Seek to the given offset, in audio samples. * returns a pair
      [sample,toskip] where * [sample] is the new current (mp4) sample * and
      [toskip] is an amount of audio sample * that should be skipped. *)
  val seek : t -> track -> int -> int * int

  val samples : t -> track -> int
  val read_sample : t -> track -> sample -> string
  val decode : t -> track -> sample -> decoder -> float array array
  val metadata : t -> (string * string) array
end
