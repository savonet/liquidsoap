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

open Content_base

(* Core reads this from the settings; a process that only typechecks scripts
   has none. *)
let default_channels = ref (fun () -> 16)

module Specs = struct
  type kind = [ `Midi ]
  type params = { channels : int }

  let name = "midi"
  let kind = `Midi
  let string_of_kind = function `Midi -> "midi"
  let kind_of_string = function "midi" -> Some `Midi | _ -> None
  let string_of_params { channels } = Printf.sprintf "channels=%d" channels
  let serialize_params { channels } = string_of_int channels

  let parse_params s =
    Option.map (fun channels -> { channels }) (int_of_string_opt s)

  let parse_param label value =
    match (label, value) with
      | "channels", c -> Some { channels = int_of_string c }
      | _ | (exception _) -> None

  let merge p p' =
    assert (p.channels = p'.channels);
    p

  let compatible p p' = p.channels = p'.channels
  let default_params _ = { channels = !default_channels () }

  let content_lang_typ =
    let open Liquidsoap_lang in
    Lang_core.record_t [("channels", Type.make Type.Int)]

  let params_to_value { channels } =
    let open Liquidsoap_lang in
    Lang_core.record [("channels", Lang_core.mk (`Int channels))]
end

module Format = MkFormatBase (Specs)
include Format

let kind = lift_kind `Midi
