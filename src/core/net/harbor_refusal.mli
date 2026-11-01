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

(** Refusals that harbor admission callbacks hand back, for listeners
    (output.harbor) and source clients (input.harbor), written with an empty
    body. *)

type t = { status : int; reason : string; headers : (string * string) list }

(** The complete reply of a refusal. *)
val reply : protocol:string -> t -> string

(** Why a header cannot be sent, if it cannot. *)
val header_error : string * string -> string option

(** Why a refusal cannot be sent, if it cannot. *)
val error : t -> string option

(** The script type of a refusal: [{status, reason, headers?}]. *)
val t : Type.t

(** A validated refusal from its script value. Raises [Error.Invalid_value] when
    it cannot be sent. *)
val of_value : Value.t -> t
