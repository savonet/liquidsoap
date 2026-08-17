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

open Parsed_term

type processor =
  ( Parser.token * Lexing.position * Lexing.position,
    Parsed_term.t )
  MenhirLib.Convert.revised

exception Includer_error of (exn * Sedlexing.lexbuf * Printexc.raw_backtrace)

let program = MenhirLib.Convert.Simplified.traditional2revised Parser.program

(* Every program starts with a `liquidsoap.script.path` binding, so that it
   scopes over the whole script. *)
let let_script_path ~filename ({ Parsed_term.pos; _ } as block) =
  let binding =
    Parser_helper.mk_stmt ~pos
      (`Binding
         {
           Parsed_term.kind = `Let;
           decoration = `None;
           pat =
             {
               pat_pos = pos;
               pat_entry = `PVar ["liquidsoap"; "script"; "path"];
             };
           arglist = None;
           cast = None;
           def = Parser_helper.mk ~pos filename;
         })
  in
  match block.Parsed_term.term with
    | `Block b ->
        {
          block with
          Parsed_term.term =
            `Block { b with block_body = binding :: b.block_body };
        }
    | _ -> assert false

let mk_expr ?fname processor lexbuf =
  let tokenizer = Preprocessor.mk_tokenizer ?fname lexbuf in
  Parser_helper.clear_comments ();
  let parsed_term = processor tokenizer in
  Parser_helper.attach_comments parsed_term;
  match fname with
    (* This happens with the interactive top-level. *)
    | None when processor != program -> parsed_term
    | None -> let_script_path ~filename:`Null parsed_term
    | Some fname ->
        let_script_path
          ~filename:(`String ('"', Lang_string.escape_utf8_string fname))
          parsed_term

(* An `%include_extra` naming a file that is not installed. The include then
   contributes no statements, which is how the minimal distributions build. *)
exception No_extra

let includer_reducer ~pos = function
  | `Include { inc_type; inc_name; inc_pos } -> (
      try
        let fname =
          match inc_type with
            | `Lib -> Filename.concat (!Hooks.liq_libs_dir ()) inc_name
            | v -> (
                try
                  let current_dir =
                    Filename.dirname (fst inc_pos).Lexing.pos_fname
                  in
                  Utils.check_readable ~current_dir
                    ~pos:[Pos.of_lexing_pos inc_pos]
                    inc_name
                with _ when v = `Extra -> raise No_extra)
        in
        let fname =
          match fname with "-" -> fname | _ -> FilePath.reduce fname
        in
        let ic = if fname = "-" then stdin else open_in fname in
        Fun.protect
          ~finally:(fun () -> if fname <> "-" then close_in ic)
          (fun () ->
            let lexbuf = Sedlexing.Utf8.from_channel ic in
            if fname <> "-" then Sedlexing.set_filename lexbuf fname;
            try mk_expr ~fname program lexbuf
            with (Parser.Error | Parsing.Parse_error) as exn ->
              let bt = Printexc.get_raw_backtrace () in
              raise (Includer_error (exn, lexbuf, bt)))
      with No_extra ->
        Parsed_term.make ~pos
          (`Block { Parsed_term.block_body = []; block_pos = pos }))

(* The included program is a `Block`. Its statements are concatenated into the
   including block, so a binding in an included file scopes over the code that
   follows the `%include`. *)
let included_statements ~pos ast =
  match (includer_reducer ~pos ast).Parsed_term.term with
    | `Block b -> b.block_body
    | _ -> assert false

(** Expand every `%include` in [tm] and normalize `Inline_if` into `If`.
    Everything else is left alone: the recursion over the other constructors is
    [Parsed_term.map_children], with the block hook doing the splicing. *)
let rec expand_term tm =
  let ast =
    match tm.Parsed_term.term with `Inline_if p -> `If p | ast -> ast
  in
  {
    tm with
    Parsed_term.term =
      Parsed_term.map_children ~block:expand_block expand_term ast;
  }

and expand_block b =
  {
    b with
    Parsed_term.block_body = List.concat_map expand_statement b.block_body;
  }

and expand_statement stmt =
  match stmt.Parsed_term.stmt with
    | `Include _ as ast ->
        List.concat_map expand_statement
          (included_statements ~pos:stmt.Parsed_term.stmt_pos ast)
    | _ -> [Parsed_term.map_statement ~block:expand_block expand_term stmt]
