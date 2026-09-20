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

(* Sources, clocks and encoders are what liquidsoap's core adds to the
   language, and a script names them in its type annotations. The typing
   environment carries the types of a source and of a clock, written by a
   liquidsoap that has them; an encoder's own type is left unknown, while its
   parameters are still checked. *)

let copy t = Type.Fresh.(make (init ~preserve_positions:true ()) t)
let find env name = Option.map (fun (_, t) -> t) (List.assoc_opt name env)

let type_of env name ?pos () =
  match find env name with
    | Some t -> copy { t with Type.pos }
    | None -> Type.var ?pos ()

let install ~env () =
  let source_ty = type_of env Reserved.source_ty in
  (Hooks.type_of_encoder := fun ~pos _ -> Type.var ?pos ());
  (Hooks.mk_source_ty :=
     fun ?pos _ _ -> source_ty ?pos:(Option.map Pos.of_lexing_pos pos) ());
  (Hooks.mk_clock_ty :=
     fun ?pos () ->
       type_of env Reserved.clock_ty ?pos:(Option.map Pos.of_lexing_pos pos) ());
  Hooks.source_methods_t := fun () -> source_ty ()
