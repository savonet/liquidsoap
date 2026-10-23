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

(** The inline producer (inline-codecs.md §3): a source that reads its input in
    a child clock and converts each input frame through one transform at a time.
    The inline operators create it with {!create} and a transform builder; the
    producer builds a transform on the first input frame after each end, relays
    the input's metadata and track marks by input time, drains the transform at
    end of input, discards it at sleep and seek, and handles the source faults
    of §15.

    A transform raises {!Source_fault} from [push] or [drain] for a fault of the
    source it converts; anything else it raises is reported the same way. Its
    item faults it counts itself. *)

exception Source_fault of string

(** Where a transform writes. [place t] writes every pending metadata and track
    mark whose input time is at or before [t]; a transform calls it with an
    output item's input time immediately before putting that item. *)
type output = {
  field : Frame.field;
  buffer : Decoder.buffer;
  place : int -> unit;
}

(** [base] is the input time of the transform's first input frame, in main
    ticks: the record's elapsed time for an encoder joining a source-id record,
    0 otherwise. [push ~input_time data] converts the field content of one input
    frame starting at [input_time]. [drain] ends the transform at an end of
    input or a source fault and [discard] at a sleep; a seek calls [close] then
    [discard]. *)
type transform = {
  base : int;
  push : input_time:int -> Content.data -> unit;
  drain : unit -> unit;
  discard : unit -> unit;
  close : unit -> unit;
}

type builder = output -> input:Source.source -> transform

(** [input_t] is the type of the input track: the child is typed
    [{field = input_t}] with an open row. *)
val create :
  name:string ->
  field:Frame.field ->
  input_t:Type.t ->
  check:(unit -> unit) option ->
  builder:builder ->
  Lang.value ->
  Source.source
