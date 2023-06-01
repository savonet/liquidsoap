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

let rec append_exported ~pos tm =
  match tm.Term.term with
    | Term.(Let ({ body } as let_t)) ->
        Term.
          { tm with term = Let { let_t with body = append_exported ~pos body } }
    | _ ->
        Parser_helper.(
          mk_let ~pos
            (let_args ~pat:(PVar ["_"]) ~def:tm ())
            (mk ~pos (Var "_export_")))

let import fname =
  let dir = !Hooks.liq_libs_dir () in
  let file = Filename.concat dir fname in
  let fd = open_in_bin file in
  let lexbuf = Sedlexing.Utf8.from_channel fd in
  Fun.protect
    ~finally:(fun () -> close_in_noerr fd)
    (fun () ->
      let processor =
        MenhirLib.Convert.Simplified.traditional2revised Parser.program
      in
      let tokenizer = Preprocessor.mk_tokenizer ~fname ~pwd:dir lexbuf in
      let term = processor tokenizer in
      let pos =
        Option.value
          ~default:(Lexing.dummy_pos, Lexing.dummy_pos)
          term.Term.t.Type.pos
      in
      let term =
        Parser_helper.(
          mk_let ~pos
            (let_args ~pat:(PVar ["_export_"]) ~def:(mk ~pos (Tuple [])) ())
            term)
      in
      let term = append_exported ~pos term in
      Typechecking.check ~throw:(fun exn -> raise exn) ~ignored:true term;
      Term.check_unused ~throw:(fun exn -> raise exn) ~lib:false term;
      term)

let () = Parser_helper.import := import
