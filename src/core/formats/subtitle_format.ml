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

module Specs = struct
  type kind = [ `Subtitle ]
  type params = unit

  let name = "subtitle"
  let kind = `Subtitle
  let string_of_kind _ = "subtitles"
  let kind_of_string = function "subtitles" -> Some `Subtitle | _ -> None
  let string_of_params () = ""
  let compatible () () = true
  let default_params _ = ()
  let parse_param _ _ = None
  let serialize_params () = ""
  let parse_params = function "" -> Some () | _ -> None
  let merge () () = ()
  let content_lang_typ = Liquidsoap_lang.Lang_core.string_t
  let params_to_value () = Liquidsoap_lang.Lang_core.string ""
end

module Format = MkFormatBase (Specs)
include Format

let format = lift_params ()
