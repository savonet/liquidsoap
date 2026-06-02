(*
  Copyright (C) 2003  Bardur Arantsson
  Copyright (C) 2004-2010 The Savonet Team

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** OCaml interface to the ao library. *)

(** Device type. *)
type t

(** Which kind of driver? *)
type driver_kind_t = [ `LIVE | `FILE | `UNKNOWN ]

(** Byte format specifier. *)
type byte_format_t = [ `LITTLE_ENDIAN | `BIG_ENDIAN | `NATIVE | `UNKNOWN ]

(** Driver type (private). *)
type driver_t = private {
  id : int;
  kind : driver_kind_t;
  short_name : string;
  name : string;
  comment : string;
  author : string;
  priority : int;
  preferred_byte_format : byte_format_t;
  options : string list;
}

(** Raised when trying to play or close * a closed device. *)
exception Closed

(** Raised when passing an invalid parameter's value. *)
exception Invalid_value

(** Get default driver *)
val get_default_driver : unit -> driver_t

(** [drivers] is a list of available drivers. *)
val drivers : driver_t list

(** [open_live]. * The [channels_matrix] parameter is * used only if the module
    is compiled against * libao >= 1.0. *)
val open_live :
  ?bits:int ->
  ?rate:int ->
  ?channels:int ->
  ?channels_matrix:string ->
  ?byte_format:byte_format_t ->
  ?options:(string * string) list ->
  ?driver:driver_t ->
  unit ->
  t

(** [open_file]. * The [channels_matrix] parameter is * used only if the module
    is compiled against * libao >= 1.0. *)
val open_file :
  ?bits:int ->
  ?rate:int ->
  ?channels:int ->
  ?channels_matrix:string ->
  ?byte_format:byte_format_t ->
  ?options:(string * string) list ->
  ?driver:driver_t ->
  ?overwrite:bool ->
  string ->
  t

(** [find_driver name] returns the driver associated with the given {b short} *
    name. *)
val find_driver : string -> driver_t

(** [play device buf] plays the sequence of samples in [buf]. *)
val play : t -> string -> unit

(** [close device] closes the given device. *)
val close : t -> unit

(** Backward compatibility functions, do not use them in new code. *)

val driver_kind : driver_t -> driver_kind_t
val driver_name : driver_t -> string
val driver_short_name : driver_t -> string
val driver_comment : driver_t -> string
val driver_author : driver_t -> string
val driver_priority : driver_t -> int
val driver_preferred_byte_format : driver_t -> byte_format_t
val driver_options : driver_t -> string list
