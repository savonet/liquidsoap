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

(** Term constructors and small utilities shared by every reducer. *)

open Parsed_term
include Runtime_term

let report_annotations ~throw ~pos annotations =
  List.iter
    (function
      | `Deprecated s ->
          let bt = Printexc.get_callstack 0 in
          throw ~bt (Term.Deprecated (s, Pos.of_lexing_pos pos)))
    annotations

let parse_error ~pos msg = raise (Term.Parse_error (pos, msg))
let render_string ~pos ~sep s = Lexer.render_string ~pos ~sep s
let mk ?pos = Term.make ?pos:(Option.map Pos.of_lexing_pos pos)
let mk_ty ?pos = Type.make ?pos:(Option.map Pos.of_lexing_pos pos)
let mk_var ?pos = Type.var ?pos:(Option.map Pos.of_lexing_pos pos)
let mk_parsed = Parsed_term.make

let mk_fun ~pos arguments body =
  mk ~pos (`Fun Term.{ free_vars = None; name = None; arguments; body })

let mk_source_ty ?pos name args =
  let fn = !Hooks.mk_source_ty in
  fn ?pos name args

let mk_clock_ty ?pos () =
  let fn = !Hooks.mk_clock_ty in
  fn ?pos ()
