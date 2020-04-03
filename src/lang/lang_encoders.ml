(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_values

(** Errors *)

exception Error of (term * string)

let invalid t =
  match t.term with Int _ | Bool _ | Float _ | String _ -> false | _ -> true

let generic_error t : exn =
  if invalid t then (
    match t.term with
      | Var _ -> Error (t, "variables are forbidden in encoding formats")
      | _ -> Error (t, "complex expressions are forbidden in encoding formats")
    )
  else Error (t, "unknown parameter name or invalid parameter value")
