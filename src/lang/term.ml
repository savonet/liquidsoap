(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

include Term_base

module ActiveTerm = Weak.Make (struct
  type typ = t
  type t = typ

  let equal t t' = t == t'
  let hash = Hashtbl.hash
end)

let active_terms = ActiveTerm.create 1024

let trim_runtime_types () =
  ActiveTerm.iter (fun term -> term.t <- Type.deep_demeth term.t) active_terms

(** Create a new value. *)
let make ?pos ?t ?methods e =
  let term = make ?pos ?t ?methods e in
  let t = match t with Some t -> t | None -> Type.var ?pos () in
  if Lazy.force debug then
    Printf.eprintf "%s (%s): assigned type var %s\n"
      (Pos.Option.to_string t.Type.pos)
      (try to_string term with _ -> "<?>")
      (Repr.string_of_type t);
  ActiveTerm.add active_terms term;
  term
