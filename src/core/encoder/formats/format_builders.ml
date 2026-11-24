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

let builders : (string * (Hooks.encoder_params -> Format_value.t)) list ref =
  ref []

let register name build = builders := (name, build) :: !builders
let find name = List.assoc name !builders
