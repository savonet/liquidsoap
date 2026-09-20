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

(** Implementations of the hooks that liquidsoap's core fills in, for the tools
    that link the language alone. Without them, typechecking a script that uses
    an encoder, or annotates a source or a clock, fails. *)

(** [env] is the typing environment the analysis checks against, which carries
    the types of a source and of a clock. *)
val install :
  env:(string * Liquidsoap_lang_types.Type.scheme) list -> unit -> unit
