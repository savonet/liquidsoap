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

(** The builder of each format name: what turns the parameters of a [%name(...)]
    expression into a format value. *)

let raise_error = Encoder_types.raise_error

let raise_generic_error = function
  | `Anonymous s -> raise_error ~pos:None ("Unknown encoder parameter: " ^ s)
  | `Labelled (l, v) ->
      raise_error ~pos:(Value.pos v)
        (Printf.sprintf
           "unknown parameter name (%s) or invalid parameter value (%s)" l
           (Value.to_string v))
  | `Encoder _ -> raise_error ~pos:None "unexpected subencoder"

(* The id3v2 parameter of the MPEG audio formats. *)
let id3v2 : Value.t -> int option Encoder_formats.adaptable = function
  | Value.Bool { value = true; _ } -> `Explicit (Some 3)
  | Value.Bool { value = false; _ } | Value.String { value = "none"; _ } ->
      `Explicit None
  | Value.Int { value = (3 | 4) as version; _ } -> `Explicit (Some version)
  | value ->
      raise_error ~pos:(Value.pos value)
        "id3v2 is true, false, \"none\", or the version 3 or 4"

let builders : (string * (Hooks.encoder_params -> Format_value.t)) list ref =
  ref []

let find name = List.assoc name !builders

(** Whether this build can encode a format. Whoever links the encoders
    implements it; a process that only typechecks takes every format. *)
let supported : (Format_value.t -> bool) ref = ref (fun _ -> true)

(** A name is available when the build can encode its default format; a name
    with no builder is not. *)
let available name =
  match find name with
    | build -> !supported (build [])
    | exception Not_found -> false

(** Registers a builder whose format is typed some other way. *)
let register_builder name build = builders := (name, build) :: !builders

(* Parameters that decide the format's type: they must be literals. *)
let type_deciding = ["stereo"; "mono"; "channels"; "video"]

let literal_value (tm : Term.t) =
  let pos = tm.Term.t.Type.pos in
  match tm.Term.term with
    | `Int i -> Some (Value.make ?pos (`Int i))
    | `Float f -> Some (Value.make ?pos (`Float f))
    | `Bool b -> Some (Value.make ?pos (`Bool b))
    | `String s -> Some (Value.make ?pos (`String s))
    | _ -> None

(* The parameters given as literals, as the builder reads them, and whether
   any was left out. *)
let rec literal_params (params : Term.encoder_params) =
  List.fold_right
    (fun param (literals, complete) ->
      match param with
        | `Anonymous s -> (`Anonymous s :: literals, complete)
        | `Encoder (name, params) ->
            let params, sub_complete = literal_params params in
            (`Encoder (name, params) :: literals, complete && sub_complete)
        | `Labelled (label, tm) -> (
            match literal_value tm with
              | Some v -> (`Labelled (label, v) :: literals, complete)
              | None when List.mem label type_deciding ->
                  raise_error ~pos:tm.Term.t.Type.pos
                    (Printf.sprintf
                       "Invalid value %s for %s. Only literal values are \
                        allowed."
                       (Term.to_string tm) label)
              | None -> (literals, false)))
    params ([], true)

let rec type_deciding_params params =
  List.filter_map
    (function
      | `Labelled (label, _) when not (List.mem label type_deciding) -> None
      | `Encoder (name, params) ->
          Some (`Encoder (name, type_deciding_params params))
      | param -> Some param)
    params

(* The type is the built format's: the builder is the one reading of the
   parameters. A literal the builder refuses fails type checking. When computed
   parameters were left out, a refusal may come from a default standing in for
   one of them, and the type is taken from the type-deciding parameters. *)
let type_of_params build params =
  Encoder_types.reject_pcm_kind_params params;
  let literals, complete = literal_params params in
  let format =
    try build literals
    with Runtime_error.Runtime_error _ when not complete ->
      build (type_deciding_params literals)
  in
  if not (!supported format) then
    raise (Term.Unsupported_encoder (None, Format_value.to_string format));
  Format_value.type_of_format ~pos:None format

(** Registers the builder of a format name, which also types it. *)
let register name build =
  register_builder name build;
  Encoder_types.register name (type_of_params build)
