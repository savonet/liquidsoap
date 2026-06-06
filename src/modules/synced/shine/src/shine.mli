(*
 * Copyright 2003-2010 Savonet team
 *
 * This file is part of Ocaml-shine.
 *
 * Ocaml-shine is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-shine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-shine; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** OCaml bindings for the libshine. *)

type t
type parameters = { samplerate : int; channels : int; bitrate : int }

exception Invalid_buffer_size

(** Raised when samplerate and/or bitrate * is invalid. *)
exception Invalid_configuration

exception Invalid_channels

val create : parameters -> t
val samples_per_pass : t -> int
val encode_buffer : t -> float array array -> string
val encode_s16le : t -> string -> int -> string
val flush : t -> string
