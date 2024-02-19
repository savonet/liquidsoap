(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

(** Runtime error, should eventually disappear. *)
exception Invalid_value of Value.t * string

exception Clock_conflict of (Pos.Option.t * string * string)
exception Clock_loop of (Pos.Option.t * string * string)

let () =
  Printexc.register_printer (function
    | Clock_conflict (pos, a, b) ->
        let pos = Pos.Option.to_string pos in
        Some
          (Printf.sprintf
             "Clock_conflict: At position: %s, a source cannot belong to two \
              clocks (%s, %s)"
             pos a b)
    | Clock_loop (pos, a, b) ->
        let pos = Pos.Option.to_string pos in
        Some
          (Printf.sprintf
             "Clock_loop: At position: %s, cannot unify two nested clocks \
              (%s,%s)"
             pos a b)
    | _ -> None)
