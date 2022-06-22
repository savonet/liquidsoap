(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

(** Generic content registration API. *)

type 'a chunk = { data : 'a; offset : int; length : int }
type ('a, 'b) chunks = { mutable params : 'a; mutable chunks : 'b chunk list }

module Contents : sig
  type kind
  type format
  type data
end

(* Raised during any invalid operation below. *)
exception Invalid

(* Raised when calling [merge] below. *)
exception Incompatible_format of Contents.format * Contents.format

type internal_content_type = [ `None | `Audio | `Video | `Midi ]

module type ContentSpecs = sig
  type kind
  type params
  type data

  (** Data *)

  val internal_content_type : internal_content_type option

  (* Size is in main ticks. *)
  val make : length:int -> params -> data

  (* TODO: This will be removed when reworking
     the streaming API. *)
  val blit : data -> int -> data -> int -> int -> unit
  val copy : data -> data

  (* TODO: this will be removed when rewriting
     streaming API. *)
  val clear : data -> unit

  (** Params *)

  val params : data -> params
  val merge : params -> params -> params
  val compatible : params -> params -> bool
  val string_of_params : params -> string

  (* [parse_param "label" "value"] *)
  val parse_param : string -> string -> params option

  (** Kind *)

  val kind : kind
  val default_params : kind -> params
  val string_of_kind : kind -> string
  val kind_of_string : string -> kind option
end

module type Content = sig
  include ContentSpecs

  (** Data *)

  val is_data : Contents.data -> bool
  val lift_data : length:int -> data -> Contents.data
  val get_data : Contents.data -> data
  val get_chunked_data : Contents.data -> (params, data) chunks

  (** Format *)

  val is_format : Contents.format -> bool
  val lift_params : params -> Contents.format
  val get_params : Contents.format -> params

  (** Kind *)

  val is_kind : Contents.kind -> bool
  val lift_kind : kind -> Contents.kind
  val get_kind : Contents.kind -> kind
end

module MkContent (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type params = C.params
     and type data = C.data

type format = Contents.format
type kind = Contents.kind
type data = Contents.data

(** Data *)

val make : length:int -> format -> data
val blit : data -> int -> data -> int -> int -> unit
val fill : data -> int -> data -> int -> int -> unit
val sub : data -> int -> int -> data
val copy : data -> data
val clear : data -> unit
val length : data -> int
val append : data -> data -> data
val is_empty : data -> bool

(** Format *)

val format : data -> format
val duplicate : format -> format
val merge : format -> format -> unit
val compatible : format -> format -> bool
val string_of_format : format -> string

(* [parse_param kind "label" "value"] *)
val parse_param : kind -> string -> string -> format

(** Kind *)

val kind : format -> kind
val default_format : kind -> format
val string_of_kind : kind -> string
val kind_of_string : string -> kind

(** Internal content types. *)

module NoneSpecs : ContentSpecs with type data = unit

(* None content type is abstract and only used
   via its params and data. *)
module None : sig
  val data : Contents.data
  val format : Contents.format
  val is_format : Contents.format -> bool
end

val is_internal_kind : kind -> bool
val is_internal_format : format -> bool

(* Some tools *)
val merge_param : name:string -> 'a option * 'a option -> 'a option
val print_optional : (string * string option) list -> string
