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

(** Names the language binds for its own use. None of them can be lexed as a
    variable (see [Lexer.is_var]), so no script can define or shadow them. *)

(** The module behind [null.m] and [null(x)]. *)
val null : string

(** A value of any type, used by the reducer as a placeholder. Scripts spell it
    [💣], which they could also redefine. *)
val any : string

(** The function behind [let eval]. *)
val eval : string

(** The parsers behind the [let json.parse] family. *)
val json_parser : string

val xml_parser : string
val yaml_parser : string
val sqlite_row_parser : string

(** Variables the reducer introduces to destructure a value, and to name the
    pattern arguments of [%argsof]. *)
val pattern_var : int -> string

val annotation_var : int -> string
