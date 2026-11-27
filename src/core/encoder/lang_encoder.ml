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

open Liquidsoap_lang
open Term

module V = Lang_core.MkCustom (struct
  type content = Encoder.format

  let name = "encoder"
  let to_string = Encoder.string_of_format

  let to_json ~pos _ =
    Runtime_error.raise ~pos
      ~message:(Printf.sprintf "Encoders cannot be represented as json")
      "json"

  let compare = Stdlib.compare
end)

module L = struct
  let format_t = Encoder_types.format_t
  let to_format = V.of_value
  let format = V.to_value
end

let raise_error = Format_builders.raise_error
let raise_generic_error = Format_builders.raise_generic_error
let register = Format_builders.register_builder
let available = Format_builders.available

let make_encoder ~pos ((e, p) : Hooks.encoder) =
  try
    let e = (Format_builders.find e) p in
    Format_value.check_complete ~pos e;
    let (_ : Encoder.factory) = Encoder.get_factory e in
    V.to_value ?pos e
  with Not_found -> raise_error ~pos "unsupported format"
