(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

open Types
open Lang_lexer

let type_and_run ast =
  Lang.check ast ;
  ignore (Lang.eval ast)

let from_in_channel stdin =
  let lexbuf = Lexing.from_channel stdin in
  let print_error error =
    flush_all () ;
    let start = lexbuf.Lexing.lex_curr_p in
      Printf.printf "Line %d, char %d"
        start.Lexing.pos_lnum
        (1+start.Lexing.pos_cnum-start.Lexing.pos_bol) ;
      if Lexing.lexeme lexbuf = "" then
        Printf.printf ": %s\n" error
      else
        Printf.printf
          " before %S: %s\n" (Lexing.lexeme lexbuf) error
  in
    try
      type_and_run (Lang_parser.scheduler Lang_lexer.token lexbuf)
    with
      | Failure "lexing: empty token" -> print_error "Empty token" ; exit 1
      | Parsing.Parse_error -> print_error "Parse error" ; exit 1
      | Lang.Unbound s -> print_error
          (Printf.sprintf
             "Unbound symbol %s!" s) ;
          exit 1
      | Lang.Invalid_value (value,msg) ->
          flush_all () ;
          Printf.printf
            "%s: %s!\n"
            (Lang.print_pos value.Lang.pos)
            msg ;
          exit 1
      | Lang.Unification_failed (pos,kind,wanted) ->
          flush_all () ;
          Printf.printf
            "%s: This value has type %s but is expected to have type %s!\n"
            (Lang.print_pos pos)
            (Lang.print_kind kind)
            (Lang.print_kind wanted) ;
          exit 1
      | e -> print_error "Unknown error" ; raise e

let from_file filename =
  let ic = open_in filename in
    from_in_channel ic ;
    close_in ic
