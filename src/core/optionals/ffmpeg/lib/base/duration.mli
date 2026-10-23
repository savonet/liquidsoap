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

(** The duration helper (common.md §6): turns timestamped items into chunks of
    positive length in main ticks, each length a difference of converted
    absolute timestamps.

    Its users (copy and raw decoders, bitstream filters, inline encoders, filter
    graph inputs and outputs) {!push} every item, pass on each chunk, and at
    every event that ends the helper {!flush} it and pass the result on, or drop
    it without flushing at a seek or after an error of the stage feeding it. *)

type 'item accessors = {
  get_ts : 'item -> Int64.t option;
  set_ts : 'item -> Int64.t option -> unit;
  get_duration : 'item -> Int64.t option;
}

type 'item t

(** [offset] is added to every timestamp read, in [src]; [start] is the position
    of the first timestamped item, in main ticks. Drops are counted against
    [faults]. *)
val create :
  ?offset:int ->
  ?start:int ->
  convert_ts:bool ->
  src:Time_base.t ->
  faults:Faults.t ->
  'item accessors ->
  'item t

type 'item chunk = { length : int; items : 'item list }

type 'item result =
  [ `Chunk of 'item chunk | `Nothing | `Dropped | `Discontinuity of string ]

(** @raise Failure when the helper is ended. *)
val push : 'item t -> 'item -> 'item result

(** Ends the helper; the held group as a chunk, if any. *)
val flush : 'item t -> 'item chunk option

(** The last timestamp read, in [src], offset included. *)
val last_ts : 'item t -> int option

(** On an ended helper, the position after the last chunk it emitted. *)
val end_position : 'item t -> int

val max_group_items : int
