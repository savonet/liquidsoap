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

(** Typecheck a script without running it, for editor tooling. Nothing here
    prints or exits. *)

type diagnostic = {
  severity : [ `Warning | `Error ];
  code : int;
  pos : Pos.t option;
  message : string;
}

type env = (string * Liquidsoap_lang_types.Type.scheme) list

type result = {
  diagnostics : diagnostic list;
  term : Term.t option;
      (** The typed script, when it parsed. Each type error in it is replaced
          with a placeholder that typechecks as any type. *)
  file : string;  (** The script's name, which its own positions carry. *)
}

(** Raises [Failure] unless [dump] was written by liquidsoap [version]. *)
val load_env : version:string -> string -> env

(** [file] names the script, so that [%include] resolves next to it. *)
val check : ?file:string -> env:env -> string -> result

(** The type of the innermost subterm at a 1-based [line] and a byte [column].
*)
val type_at : result -> line:int -> column:int -> string option

(** The names the script binds around a position, in enclosing definitions and
    functions. They shadow the environment's names of the same spelling. *)
val locals_at : result -> line:int -> column:int -> string list

(** The names a script can write that are in scope at a position: the
    environment's, and {!locals_at}. *)
val scope_at : env:env -> result -> line:int -> column:int -> string list

(** The methods of the innermost subterm's type at a position, with their types.
*)
val methods_at : result -> line:int -> column:int -> (string * string) list

(** The methods of [null.m], which no script can shadow. *)
val null_methods : env:env -> (string * string) list
