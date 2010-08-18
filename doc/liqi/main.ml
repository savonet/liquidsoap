(*****************************************************************************

  Liqi, a simple wiki-like langage
  Copyright 2008-2010 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Liqi_parser

let pp =
  let state = ref [] in
  let push x = state := x :: !state in
    fun lexbuf ->
      let rec emit s =
        let n = String.length s in
        if n>2 && s.[0]='*' && s.[1]='*' then begin
          push (WORD (String.sub s 2 (n-2))) ;
          LBF
        end else if s.[0]='_' then begin
          push (WORD (String.sub s 1 (n-1))) ;
          LEM
        end else if n>2 && s.[n-1]='*' && s.[n-2]='*' then begin
          push RBF ;
          emit (String.sub s 0 (n-2))
        end else if s.[n-1]='_' then begin
          push REM ;
          emit (String.sub s 0 (n-1))
        end else
          WORD s
      in
      match !state with
        | WORD s :: l -> state := l ; emit s
        | s :: l -> state := l ; s
        | [] ->
            begin match Liqi_lexer.token lexbuf with
              | WORD s -> emit s
              | t -> t
            end

type fmt = [`HTML | `LATEX_FULL | `LATEX]

let infile = ref "stdin"
let outfile = ref "stdout"
let outfmt = (ref `HTML : fmt ref)
let main = ref false
let main_main = ref false
let basedir = ref ""

let () =
  let () =
    Arg.parse
      [
        "-i", Arg.Set_string infile, "Input file.";
        "-o", Arg.Set_string outfile, "Output file.";
        "--latex",
        Arg.Unit (fun () -> outfmt := `LATEX),
        "Output LaTeX files (for inclusion).";
        "--main", Arg.Set main, "Generate for main Savonet website.";
        "--main-main",
        Arg.Set main_main,
        "Generate for main Savonet website's frontpage.";
	"--basedir",
	Arg.Set_string basedir,
	"Basedir (to compute relative paths).";
        "--latex-full",
        Arg.Unit (fun () -> outfmt := `LATEX_FULL), "Output LaTeX files.";
      ]
      (fun _ -> ())
      "liqi [arguments]"
  in
  let inchan =
    if !infile = "stdin" then
      stdin
    else
      open_in !infile
  in
  let lexbuf = Lexing.from_channel inchan in
  let print_error error =
    flush_all () ;
    let start = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "%sine %d, char %d"
        (if start.Lexing.pos_fname="" then "L" else
           Printf.sprintf "File %S, l" start.Lexing.pos_fname)
        start.Lexing.pos_lnum
        (1+start.Lexing.pos_cnum-start.Lexing.pos_bol) ;
      if Lexing.lexeme lexbuf = "" then
        Printf.eprintf ": %s\n" error
      else
        Printf.eprintf
          " before %S: %s.\n" (Lexing.lexeme lexbuf) error
  in
  let title, doc =
    try Liqi_parser.doc_title pp lexbuf with
      | Parsing.Parse_error -> print_error "Syntax error" ; exit 1
      | e -> print_error (Printexc.to_string e) ; exit 1
  in
  let outchan =
    if !outfile = "stdout" then
      stdout
    else
      open_out !outfile
  in
    match !outfmt with
      | `HTML ->
          let printer =
            if !main_main then Html.print_main_main else
              let basedir = !basedir in
                if !main then Html.print_main ~basedir else Html.print
          in
            printer
              ?filename:(if !outfile="stdout" then None else Some !outfile)
              outchan ?title doc
      | `LATEX -> Latex.print false outchan doc
      | `LATEX_FULL -> Latex.print true outchan doc
