(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

open Term
open Term.Ground

exception Error of (Term.pos option * string)

let generic_error (l, t) : exn =
  match t with
    | `Value v ->
        Error
          ( v.Value.pos,
            Printf.sprintf
              "unknown parameter name (%s) or invalid parameter value (%s)" l
              (Value.print_value v) )
    | `Encoder _ -> Error (None, "unexpected subencoder")

(** An encoder. *)
type encoder = {
  kind_of_encoder : Term.encoder_params -> Frame.kind Frame.fields;
      (** Compute the kind of the encoder. *)
  make : Value.encoder_params -> Encoder.format;
      (** Actually create the encoder. *)
}

let encoders = ref []

(** Register an encoder. *)
let register name kind_of_encoder make =
  encoders := (name, { kind_of_encoder; make }) :: !encoders

(** Find an encoder with given name. *)
let find_encoder name = List.assoc name !encoders

let channels_of_params ?(default = 2) p =
  match
    List.find_map
      (function
        | "", `Term { term = Ground (String "stereo") } -> Some 2
        | "", `Term { term = Ground (String "mono") } -> Some 1
        | "channels", `Term { term = Ground (Int n) } -> Some n
        | _ -> None)
      p
  with
    | Some n -> n
    | None -> default

(** Compute a kind from a non-fully evaluated format. This should give the same
    result than [Encoder.kind_of_format] once evaluated... *)
let kind_of_encoder ((e, p) : Term.encoder) = (find_encoder e).kind_of_encoder p

let type_of_encoder ~pos ~level e =
  let kind = kind_of_encoder e in
  let audio = kind_t ~pos ~level kind.Frame.audio in
  let video = kind_t ~pos ~level kind.Frame.video in
  let midi = kind_t ~pos ~level kind.Frame.midi in
  format_t ~pos ~level (frame_kind_t ~pos ~level audio video midi)

let make_encoder ~pos t ((e, p) : Value.encoder) =
  try
    let e = (find_encoder e).make p in
    let (_ : Encoder.factory) = Encoder.get_factory e in
    e
  with Not_found -> raise (Term.Unsupported_format (pos, Term.print_term t))
