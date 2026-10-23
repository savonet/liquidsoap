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

(** The description of a filter graph (filters.md §7, §11.2): what a script
    states with a graph value, kept as statements replayed in order into an
    avfilter config, into the probe config at the statement's script position
    and into a fresh config at each build. Elements are filter instances, buffer
    sources and sinks; pad values designate an output pad of an element in
    whichever config is being replayed, through the element's [attached]
    instance of that config.

    The graph operators call {!add} for each statement, {!expect} and
    {!consumed} to track the pads still to link for the completeness check of
    §7.2, {!edge} for the reachability of §12.2, then {!close} when
    [ffmpeg.filter.create] returns. *)

type mode = [ `Probe | `Build ]

type element = {
  eid : int;
  name : string;
  mutable attached : [ `Attached ] Avfilter.filter option;
}

type media = [ `Audio | `Video ]
type t

(** A pad value. [pid] is its creation number, unique in the process; [resolve]
    reads the pad in the config being replayed. *)
type 'm pad = {
  pid : int;
  owner : t;
  element : element;
  media : media;
  index : int;
  resolve : unit -> ([ `Attached ], 'm, [ `Output ]) Avfilter.pad;
}

val create : gid:int -> unit -> t
val gid : t -> int

(** Whether the probe config is still open: the graph value is usable. *)
val in_scope : t -> bool

val close : t -> unit

(** A new element, named [<prefix>_<n>], unique in the graph. *)
val element : t -> string -> element

(** @raise Failure when the element has no instance in the current config. *)
val attached : element -> [ `Attached ] Avfilter.filter

(** Runs the statement on the probe config, then records it. *)
val add : t -> (mode -> Avfilter.config -> unit) -> unit

val replay : t -> Avfilter.config -> unit
val media_name : media -> string

val expect :
  t -> element -> [ `Input | `Output ] -> media -> int -> string -> unit

val consumed : t -> [ `Input | `Output ] -> element -> media -> int -> unit

(** Expects every pad of the instance attached in the probe config. *)
val expect_pads : t -> element -> [ `Attached ] Avfilter.filter -> unit

(** A pad still unlinked, described for the error of §7.2. *)
val first_unlinked : t -> string option

val edge : t -> src:element -> dst:element -> unit

(** The elements a path of links reaches from [from]. *)
val reached : t -> from:element list -> element -> bool

val pad :
  t ->
  element ->
  media ->
  int ->
  (unit -> ([ `Attached ], 'm, [ `Output ]) Avfilter.pad) ->
  'm pad

val audio_output : t -> element -> int -> [ `Audio ] pad
val video_output : t -> element -> int -> [ `Video ] pad

(** Links [src] to an input pad of [dst]. At a build, a refused link raises
    [Failure] naming both ends. *)
val link :
  t ->
  'm pad ->
  dst:element ->
  dst_name:string ->
  (unit -> ([ `Attached ], 'm, [ `Input ]) Avfilter.pad) ->
  unit
