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

(** Reduce parsed type annotations to actual types. *)

open Parsed_term
include Runtime_term
open Term_reducer_helpers

let mk_named_ty ?pos = function
  | "_" -> Type.var ?pos:(Option.map Pos.of_lexing_pos pos) ()
  | "unit" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.unit
  | "never" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Never
  | "bool" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Bool
  | "int" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Int
  | "float" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Float
  | "string" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.String
  | "ref" -> Type.reference (Type.var ())
  | "clock" -> mk_clock_ty ?pos ()
  | "source" ->
      mk_source_ty ?pos "source" (`Tracks { extensible = true; tracks = [] })
  | "source_methods" -> !Hooks.source_methods_t ()
  | name -> (
      match Type.find_opt_typ name with
        | Some c -> c ()
        | None ->
            let pos =
              Option.value ~default:(Lexing.dummy_pos, Lexing.dummy_pos) pos
            in
            raise
              (Term.Parse_error (pos, "Unknown type constructor: " ^ name ^ "."))
      )

let rec mk_parsed_ty ?pos ~env ~to_term = function
  | `Named s -> mk_named_ty ?pos s
  | `Nullable t ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (Nullable (mk_parsed_ty ?pos ~env ~to_term t)))
  | `List t ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (List { t = mk_parsed_ty ?pos ~env ~to_term t; json_repr = `Tuple }))
  | `Ref t ->
      Type.reference
        ?pos:(Option.map Pos.of_lexing_pos pos)
        (mk_parsed_ty ?pos ~env ~to_term t)
  | `Json_object t ->
      Type.(
        make
          (List
             {
               t = mk_parsed_ty ?pos ~env ~to_term (`Tuple [`Named "string"; t]);
               json_repr = `Object;
             }))
  | `Tuple l ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (Tuple (List.map (mk_parsed_ty ~env ~to_term ?pos) l)))
  | `Arrow (args, t) ->
      Type.(
        make
          (Arrow
             ( List.map
                 (fun (optional, name, t) ->
                   (optional, name, mk_parsed_ty ~env ~to_term ?pos t))
                 args,
               mk_parsed_ty ?pos ~env ~to_term t )))
  | `Record l ->
      List.fold_left (mk_meth_ty ?pos ~env ~to_term) Type.(make (Tuple [])) l
  | `Method (t, l) ->
      List.fold_left
        (mk_meth_ty ?pos ~env ~to_term)
        (mk_parsed_ty ?pos ~env ~to_term t)
        l
  | `Invoke (t, s) -> snd (Type.invoke (mk_parsed_ty ?pos ~env ~to_term t) s)
  | `Source (s, p) -> mk_source_ty ?pos s p
  | `Getter t ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (Getter (mk_parsed_ty ?pos ~env ~to_term t)))

and mk_meth_ty ?pos ~env ~to_term base
    { Parsed_term.name; optional_meth = optional; typ; json_name } =
  Type.(
    make
      (Meth
         ( {
             meth = name;
             optional;
             scheme = ([], mk_parsed_ty ?pos ~env ~to_term typ);
             doc = { meth_descr = ""; category = `Method };
             json_name;
           },
           base )))
