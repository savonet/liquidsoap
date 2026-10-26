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

(** The run layer of FFmpeg content (content.md §4.2, §6, §11): a data value is
    a length and runs, each run a stretch of one source with its items at
    positions relative to the run start. Every operation is lossless. *)

module type Item = sig
  type t

  (** The item copy of §6.7: a new packet reference, or the shared frame. *)
  val copy : t -> t

  val checksum : t -> string
end

module type Params = sig
  type t

  (** Known parameters are kept by a blit; unknown ones are taken from the
      source. *)
  val known : t -> bool
end

module type S = sig
  type item
  type params

  type run = {
    length : int;
    source_id : Source_id.t;
    continues : bool;
    time_base : Avutil.rational;
    source_params : Copy_params.t option;
    items : (int * item) list;
  }

  type data = {
    mutable params : params;
    mutable length : int;
    mutable runs : (int * run) list;
  }

  include
    Content_base.DataSpecs with type params := params and type data := data

  val run :
    ?continues:bool ->
    ?source_params:Copy_params.t ->
    source_id:Source_id.t ->
    time_base:Avutil.rational ->
    length:int ->
    (int * item) list ->
    run

  (** One run at start 0, of the run's length: what a producer hands to a
      generator. *)
  val of_run : params -> run -> data
end

module Make (P : Params) (I : Item) :
  S with type item = I.t and type params = P.t
