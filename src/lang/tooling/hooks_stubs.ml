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

open Liquidsoap_lang_types
open Liquidsoap_lang_values

(* What liquidsoap's core adds to the language and a process that only
   typechecks cannot compute: the type of a source with its methods, and the
   type of a clock. A dump carries both, written by a liquidsoap that has
   them.

   These stand in for whoever is not linked here, so a process that does
   implement one keeps its own. *)

let copy t = Type.Fresh.(make (init ~preserve_positions:true ()) t)

let dumped t ?pos () =
  match t with Some t -> copy { t with Type.pos } | None -> Type.var ?pos ()

let install ~core_types () =
  Hooks.fallback Hooks.type_of_encoder (fun ~pos _ -> Type.var ?pos ());
  Hooks.fallback Hooks.mk_clock_ty (fun ?pos () ->
      dumped core_types.Jsoo_safe_env.clock
        ?pos:(Option.map Pos.of_lexing_pos pos)
        ());
  Hooks.fallback Hooks.source_methods_t (fun () ->
      dumped core_types.Jsoo_safe_env.source_methods ())
