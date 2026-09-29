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

open Liquidsoap_lang

(* Whatever the language leaves to liquidsoap's core and a typing dump can
   answer: the content of a source's tracks is computed here, while the source
   and clock types themselves are what the dump carried. *)

let render_string = function
  | `Verbatim s -> s
  | `String (pos, (sep, s)) -> String_literal.render ~pos ~sep s

(* A track's type, as a script writes it: [pcm(stereo)], [any], [internal]. *)
let field_t ?pos kind params =
  let err_pos =
    Option.value ~default:(Lexing.dummy_pos, Lexing.dummy_pos) pos
  in
  let pos = Option.map Pos.of_lexing_pos pos in
  match kind with
    | "any" -> Type.var ?pos ()
    | "none" | "never" -> Type.make ?pos Type.Never
    | _ -> (
        try
          let k = Content_base.kind_of_string kind in
          match params with
            | [] -> Type.make ?pos (Format_type.descr (`Kind k))
            | [("", `Verbatim "any")] -> Type.var ?pos ()
            | [("", `Verbatim "internal")] ->
                Type.var ?pos ~constraints:[Format_type.internal_tracks] ()
            | param :: params ->
                let format (label, value) =
                  Content_base.parse_param k label (render_string value)
                in
                let f = format param in
                List.iter
                  (fun param -> Content_base.merge f (format param))
                  params;
                Type.make ?pos (Format_type.descr (`Format f))
        with _ ->
          let params =
            params
            |> List.map (fun (l, v) -> l ^ "=" ^ render_string v)
            |> String.concat ","
          in
          raise
            (Term.Parse_error
               ( err_pos,
                 "Unknown type constructor: " ^ kind ^ "(" ^ params ^ ")." )))

(* A source's type, with the tracks a script annotated. *)
let source_t ?pos frame_t =
  Type.make ?pos
    (Type.Constr
       { Type.constructor = "source"; params = [(`Invariant, frame_t)] })

let abstract_source_t ?pos () =
  Type.make ?pos (Type.Constr { Type.constructor = "source"; params = [] })

let mk_source_ty ?pos name annotation =
  if name <> "source" then (
    let pos = Option.value ~default:(Lexing.dummy_pos, Lexing.dummy_pos) pos in
    raise (Term.Parse_error (pos, "Unknown type constructor: " ^ name ^ ".")));
  let pos' = Option.map Pos.of_lexing_pos pos in
  match annotation with
    | `Abstract -> abstract_source_t ?pos:pos' ()
    | `Tracks { Parsed_term.extensible; tracks } -> (
        match tracks with
          | [] -> source_t ?pos:pos' (Type.var ())
          | tracks ->
              let fields =
                List.fold_left
                  (fun fields
                       { Parsed_term.track_name; track_type; track_params } ->
                    Fields.add
                      (Fields.field_of_string track_name)
                      (field_t ?pos track_type track_params)
                      fields)
                  Fields.empty tracks
              in
              let base =
                if extensible then Type.var () else Type.make Type.unit
              in
              source_t ?pos:pos' (Frame_type.make base fields))

(* Linking this library is what implements the hooks it covers. *)
let () = Hooks.implement Hooks.mk_source_ty mk_source_ty
