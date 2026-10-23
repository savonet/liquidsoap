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

(** The subtitle timeline (decoder.md §16.2): timed items into chunks on a
    sparse field. A subtitle is held at its position until the next item or the
    next [advance], and its chunk spans the gap to that point. *)

type 'data t

(** [output ~length data] puts one chunk holding [data] at offset 0, or no data.
    [delay d data] shifts [data] [d] ticks later in its own terms. *)
val create :
  merge:('data -> 'data -> 'data) ->
  delay:(int -> 'data -> 'data) ->
  output:(length:int -> 'data option -> unit) ->
  unit ->
  'data t

val item :
  'data t ->
  position:int ->
  duration:int ->
  'data ->
  [ `Ok | `Item_fault of string ]

val advance : 'data t -> int -> unit
val flush : 'data t -> unit

(** The position reached on the field. *)
val reached : 'data t -> int option
