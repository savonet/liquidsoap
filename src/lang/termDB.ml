(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

(** Optimized representation of terms with de Bruijn indices. *)

module Ground = Term.Ground

type pos = Pos.t
type var = int

type t =
  | Ground of Ground.t
  | Encoder of encoder
  | List of t list
  | Tuple of t list
  | Null
  | Cast of t * Type.t
  | Meth of string * t * t (* TODO: have an hashtbl of methods *)
  | Invoke of t * string
  | Open of t * t
  | Let of let_t
  | Var of var * string (* The string is only used for debugging. *)
  | Seq of t * t
  (* TODO: we should pre-compute applications when the type is fully known (be
     careful of subtyping!) *)
  | App of t * (string * t) list
  | Fun of (string * t option) list * t
  | RFun of (string * t option) list * t

and closure = t Lazy.t list

and let_t = {
  replace : bool;
  (* whether the definition replaces a previously existing one (keeping methods) *)
  pat : pattern;
  def : t;
  body : t;
}

and pattern =
  | PVar  (** a variable *)
  | PField of var * string list  (** a field *)
  | PTuple of pattern list  (** a tuple *)
  | PList of (pattern list * bool * pattern list)  (** a list *)
  (* TODO: it would be cleaner to have a _ pattern instead of options below *)
  | PMeth of (pattern option * (string * pattern option) list)
      (** a value with methods *)

(** Parameters for encoders. *)
and encoder_params =
  (string * [ `Term of pos option * t | `Encoder of encoder ]) list

(** A formal encoder. *)
and encoder = pos option * string * encoder_params

(** Used for printing very simple functions. *)
let is_ground = function Ground _ -> true | _ -> false

(** String representation of ground terms. *)
let string_of_ground t =
  assert (is_ground t);
  match t with Ground g -> Ground.to_string g | _ -> assert false
