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

let script_path_binding ~filename ~pos =
  Parser_helper.mk_stmt ~pos
    (`Binding
       {
         Parsed_term.kind = `Let;
         decoration = `None;
         pat =
           { pat_pos = pos; pat_entry = `PVar ["liquidsoap"; "script"; "path"] };
         arglist = None;
         cast = None;
         def = Parser_helper.mk ~pos filename;
       })

let script_path_value = function
  | Some fname -> `String ('"', Lang_string.escape_utf8_string fname)
  | None -> `Null

(* Every program starts with a `liquidsoap.script.path` binding, so that it
   scopes over the whole script. It sits at the empty end of the script, since
   spanning the whole script would place it under every position in it. *)
let let_script_path ~filename ({ Parsed_term.pos = _, stop; _ } as block) =
  let pos = (stop, stop) in
  let binding = script_path_binding ~filename ~pos in
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
    | fname -> let_script_path ~filename:(script_path_value fname) parsed_term

(* An `%include_extra` naming a file that is not installed. The include then
   contributes no statements, which is how the minimal distributions build. *)
exception No_extra

let includer_reducer ~pos = function
  | `Include { inc_type; inc_name; inc_pos } -> (
      try
        let fname =
          match inc_type with
            | `Lib -> (
                try Filename.concat (Hooks.get Hooks.liq_libs_dir ()) inc_name
                with Not_found ->
                  raise
                    (Term.Parse_error
                       ( inc_pos,
                         "Cannot include " ^ inc_name
                         ^ ": the standard library's directory is unknown." )))
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
    Parsed_term.block_body =
      expand_statements ~restore_script_path:false b.Parsed_term.block_body;
  }

(* The last statement of a block is its value, hence the one that must not be
   followed by anything. *)
and expand_statements ~restore_script_path = function
  | [] -> []
  | [stmt] -> expand_statement ~restore_script_path stmt
  | stmt :: stmts ->
      expand_statement ~restore_script_path:true stmt
      @ expand_statements ~restore_script_path stmts

and expand_statement ~restore_script_path stmt =
  match stmt.Parsed_term.stmt with
    | `Include _ as ast ->
        let pos = stmt.Parsed_term.stmt_pos in
        let included =
          expand_statements ~restore_script_path (included_statements ~pos ast)
        in
        (* The included file binds its own `liquidsoap.script.path`, which the
           splice would otherwise leave in scope for the rest of the including
           file. An absent `%include_extra` splices nothing and shadows
           nothing. *)
        if restore_script_path && included <> [] then (
          let stop = snd pos in
          let fname = (fst pos).Lexing.pos_fname in
          let filename =
            script_path_value (if fname = "" then None else Some fname)
          in
          included @ [script_path_binding ~filename ~pos:(stop, stop)])
        else included
    | _ -> [Parsed_term.map_statement ~block:expand_block expand_term stmt]
