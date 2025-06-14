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

(** Runtime reducer from parsed terms to runtime terms. *)

type processor =
  ( Parser.token * Lexing.position * Lexing.position,
    Parser_helper.Term.t )
  MenhirLib.Convert.revised

val program : processor
val typecheck : (?env:Typing.env -> Term.t -> unit) ref
val mk_expr : ?fname:string -> processor -> Sedlexing.lexbuf -> Parsed_term.t
val to_term : throw:(exn -> unit) -> Parsed_term.t -> Term.t
val to_encoder_params : Parsed_term.encoder_params -> Term.encoder_params
val needs_toplevel : unit -> bool
