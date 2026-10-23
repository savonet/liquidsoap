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

(** The inline chunker (inline-codecs.md §8): the duration helper over a
    transform's packets (DTS mode) or frames (PTS mode), writing each chunk on
    the producer's field after the metadata and track marks that precede its
    first item (§3.3). *)

type 'item t

val packets :
  faults:Faults.t ->
  src:Avutil.rational ->
  output:Producer.output ->
  to_data:(length:int -> 'media Avcodec.Packet.t list -> Content.data) ->
  unit ->
  'media Avcodec.Packet.t t

val frames :
  faults:Faults.t ->
  src:Avutil.rational ->
  output:Producer.output ->
  to_data:(length:int -> 'media Avutil.frame list -> Content.data) ->
  unit ->
  'media Avutil.frame t

(** @raise Producer.Source_fault at a timestamp discontinuity. *)
val push : 'item t -> 'item -> unit

val flush : 'item t -> unit
