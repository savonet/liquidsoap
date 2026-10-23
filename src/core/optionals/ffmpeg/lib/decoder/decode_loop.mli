(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** The decoder record of one opened container (decoder.md §6, §8–§13): the
    stream mapping, the decode loop with its pending queue and thresholds,
    positions, discontinuities, metadata, seeking, end of input and remaining
    time.

    {!create} maps the container's streams onto the requested content type; the
    generic decoder then calls [decode] until its buffer holds enough, [seek],
    and [eof] or [close]. [decode] raises only to end the track: [End_of_file]
    at the end of input, {!Aborted} when the input's interrupt aborted a read,
    {!Source_fault} when the input cannot be decoded further. The generic file
    decoder reads any of them as the end and calls [eof], which drains every
    stage and adds a track mark. *)

exception Aborted
exception Source_fault of string

type t

(** @raise Failure
      when a requested field needs a per-stream decoder this build does not
      provide. *)
val create :
  ?request_metadata:Frame.metadata ->
  decode_first_metadata:bool ->
  ctype:Frame.content_type ->
  description:Stream_description.t ->
  Avutil.input Avutil.container ->
  t

val decode : t -> Decoder.buffer -> unit
val eof : t -> Decoder.buffer -> unit

(** [seek t x] moves [x] main ticks and returns the ticks moved. *)
val seek : t -> int -> int

val close : t -> unit

(** Remaining main ticks, [-1] when the duration is unknown. *)
val remaining : t -> int

val decoder : t -> Decoder.decoder

(** Whether every stream of the container maps onto [ctype]. *)
val can_map : ctype:Frame.content_type -> Stream_description.t -> bool

(** [can_map], and every media field of [ctype] gets a stream: the check of a
    live input (io.md §2.2 step 5, §10.6). *)
val can_produce : ctype:Frame.content_type -> Stream_description.t -> bool
