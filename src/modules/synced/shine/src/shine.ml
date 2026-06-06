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

type enc
type t = { enc : enc; samples_per_pass : int }
type parameters = { samplerate : int; channels : int; bitrate : int }

exception Invalid_buffer_size
exception Invalid_configuration
exception Invalid_channels

external check_config : int -> int -> bool = "ocaml_shine_check_config"
external samples_per_pass : enc -> int = "ocaml_shine_samples_per_pass"
external create : int -> int -> int -> enc = "ocaml_shine_init"

let create params =
  if not (check_config params.samplerate params.bitrate) then
    raise Invalid_configuration;
  if params.channels < 1 || params.channels > 2 then raise Invalid_channels;
  let enc = create params.channels params.samplerate params.bitrate in
  { enc; samples_per_pass = samples_per_pass enc }

let samples_per_pass enc = enc.samples_per_pass

external encode_buffer : enc -> float array array -> string
  = "ocaml_shine_encode_float"

let encode_buffer enc buf =
  if Array.length buf == 0 || Array.length buf.(0) != enc.samples_per_pass then
    raise Invalid_buffer_size;

  encode_buffer enc.enc buf

external encode_s16le : enc -> string -> int -> string
  = "ocaml_shine_encode_s16le"

let encode_s16le enc data chans =
  if String.length data < 2 * enc.samples_per_pass * chans then
    raise Invalid_buffer_size;

  encode_s16le enc.enc data chans

external flush : enc -> string = "ocaml_shine_flush"

let flush enc = flush enc.enc
