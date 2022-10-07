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

open Liquidsoap_lang
open Term
open Term.Ground

module V = Lang_core.MkAbstract (struct
  type content = Encoder.format

  let name = "encoder"
  let descr = Encoder.string_of_format

  let to_json _ =
    raise
      Runtime_error.(
        Runtime_error
          {
            kind = "json";
            msg = Printf.sprintf "Encoders cannot be represented as json";
            pos = [];
          })

  let compare = Stdlib.compare
end)

module L = struct
  (** Type of audio formats that can encode frame of a given kind. *)
  let format_t ?pos k =
    Type.make ?pos
      (Type.Constr { Type.constructor = "format"; params = [(`Covariant, k)] })

  let to_format = V.of_value
  let format = V.to_value
end

let has_started = ref false
let () = Lifecycle.before_start (fun () -> has_started := true)

let error ~pos msg =
  match !has_started with
    | false -> Lang_error.Encoder_error (pos, msg)
    | true ->
        Runtime_error.(
          Runtime_error
            {
              kind = "encoder";
              msg;
              pos = (match pos with None -> [] | Some pos -> [pos]);
            })

let generic_error (l, t) : exn =
  match t with
    | `Value v ->
        error ~pos:v.Value.pos
          (Printf.sprintf
             "unknown parameter name (%s) or invalid parameter value (%s)" l
             (Value.to_string v))
    | `Encoder _ -> error ~pos:None "unexpected subencoder"

(** An encoder. *)
type encoder = {
  kind_of_encoder : Term.encoder_params -> Frame.kind Frame.Fields.t;
      (** Compute the kind of the encoder. *)
  make : Hooks.encoder_params -> Encoder.format;
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

let type_of_encoder ~pos e =
  let kind = kind_of_encoder e in
  let audio = Frame_type.make_kind ?pos (Frame.find_audio kind) in
  let video = Frame_type.make_kind ?pos (Frame.find_video kind) in
  let midi = Frame_type.make_kind ?pos (Frame.find_midi kind) in
  L.format_t ?pos (Frame_type.make ?pos ~audio ~video ~midi ())

let make_encoder ~pos t ((e, p) : Hooks.encoder) =
  try
    let e = (find_encoder e).make p in
    let (_ : Encoder.factory) = Encoder.get_factory e in
    V.to_value e
  with Not_found ->
    raise
      (error ~pos (Printf.sprintf "unsupported format: %s" (Term.to_string t)))
