(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let clock = Modules.clock

let _ =
  Lang.add_builtin ~base:clock "create" ~category:`Liquidsoap
    ~descr:"Create a new clock"
    [
      ( "id",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Identifier for the new clock." );
    ]
    Lang_source.ClockValue.t
    (fun p ->
      let id = Lang.to_valued_option Lang.to_string (List.assoc "id" p) in
      let pos = match Lang.pos p with p :: _ -> Some p | [] -> None in
      Lang_source.ClockValue.to_value (Clock.create ?pos ?id ()))
