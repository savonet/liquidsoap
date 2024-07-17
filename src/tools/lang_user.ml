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

(* The Lang_lexer is not quite enough for our needs,
 * so we first define convenient layers between it and the parser.
 * First a pre-processor which evaluates %ifdefs. *)
let preprocess tokenizer =
  let state = ref 0 in
  let rec token lexbuf =
    match tokenizer lexbuf with
      | Lang_parser.PP_IFDEF ->
          begin match tokenizer lexbuf with
            | Lang_parser.VAR v ->
                if List.mem_assoc v !Lang.bindings then begin
                  incr state ;
                  token lexbuf
                end else
                  let rec skip () =
                    match tokenizer lexbuf with
                      | Lang_parser.PP_ENDIF -> token lexbuf
                      | _ -> skip ()
                  in
                    skip ()
            | _ -> failwith "expected a variable after %ifdef"
          end
      | Lang_parser.PP_ENDIF ->
          if !state=0 then failwith "no %ifdef to end here" ;
          decr state ;
          token lexbuf
      | x -> x
  in
    token

type exp_item =
  | String of string | Expr of Lexing.lexbuf | Concat | RPar | LPar

(* The expander turns "bla #{e} bli" into ("bla "^e^" bli") *)
let expand tokenizer =
  let state = Queue.create () in
  let add x = Queue.add x state in
  let pop () = ignore (Queue.take state) in
  let parse s =
    let l = Pcre.split ~pat:"#{(.*?)}" s in
    let l = if l = [] then [""] else l in
    let rec parse = function
      | s::x::l ->
          List.iter add
            [ String s ; Concat; LPar ; Expr (Lexing.from_string x) ; RPar ] ;
          if l<>[] then begin
            add Concat ;
            parse l
          end
      | [x] -> add (String x)
      | [] -> assert false
    in
      parse l
  in
  let rec token lexbuf =
    if Queue.is_empty state then begin
      match tokenizer lexbuf with
        | Lang_parser.STRING s ->
            parse s ;
            if Queue.length state > 1 then begin
              add RPar ;
              Lang_parser.LPAR
            end else
              token lexbuf
        | x -> x
    end else
      match Queue.peek state with
        | String s -> pop () ; Lang_parser.STRING s
        | Concat   -> pop () ; Lang_parser.BIN2 "^"
        | RPar     -> pop () ; Lang_parser.RPAR
        | LPar     -> pop () ; Lang_parser.LPAR
        | Expr b ->
            begin match tokenizer b with
              | Lang_parser.EOF -> pop () ; token lexbuf
              | x -> x
            end
  in
    token

(* Last but not least: remove new lines and merge some tokens around them
 * in order to remove some ambiguities, typically between:
 *   def foo \n (x,y) ... << Normal definition, starting with a couple
 *   def foo(x,y) ...     << Definition of the function foo *)
let strip_newlines tokenizer =
  let state = ref None in
  let rec token lexbuf =
    match !state with
      | None ->
          begin match tokenizer lexbuf with
            | Lang_parser.PP_ENDL -> token lexbuf
            | Lang_parser.VAR _ as v ->
                state := Some v ;
                token lexbuf
            | x -> x
          end
      | Some (Lang_parser.VAR var as v) ->
          begin match tokenizer lexbuf with
            | Lang_parser.LPAR -> state := None ; Lang_parser.VARLPAR var
            | Lang_parser.LBRA -> state := None ; Lang_parser.VARLBRA var
            | Lang_parser.PP_ENDL -> state := None ; v
            | x -> state := Some x ; v
          end
      | Some x -> state := None ; x
  in
    token

(* Wrap the lexer with its extensions *)
let token = expand (preprocess (strip_newlines Lang_lexer.token))

let type_and_run ast =
  Lang.check ast ;
  ignore (Lang.eval_toplevel ast)

let from_in_channel ~ns stdin =
  let lexbuf = Lexing.from_channel stdin in
  let print_error error =
    flush_all () ;
    let start = lexbuf.Lexing.lex_curr_p in
      Printf.printf "%sine %d, char %d"
        (if start.Lexing.pos_fname="" then "L" else
           Printf.sprintf "File %S, l" start.Lexing.pos_fname)
        start.Lexing.pos_lnum
        (1+start.Lexing.pos_cnum-start.Lexing.pos_bol) ;
      if Lexing.lexeme lexbuf = "" then
        Printf.printf ": %s\n" error
      else
        Printf.printf
          " before %S: %s\n" (Lexing.lexeme lexbuf) error
  in
    assert (lexbuf.Lexing.lex_start_p = lexbuf.Lexing.lex_curr_p) ;
    begin match ns with
      | Some ns ->
          lexbuf.Lexing.lex_start_p <- { lexbuf.Lexing.lex_start_p with
                                             Lexing.pos_fname = ns } ;
          lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                             Lexing.pos_fname = ns }
      | None -> ()
    end ;
    try
      type_and_run (Lang_parser.scheduler token lexbuf)
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
      | Lang.Wrong_label (pos,lbl) ->
          flush_all () ;
          Printf.printf
            "%s: The function applied here has no argument labelled %s!\n"
            (Lang.print_pos pos)
	    lbl ;
          exit 1
      | Dtools.Var.Type_error (name,kind) ->
          print_error (Printf.sprintf "Setting %S should have type %s."
                         name
                         (match kind with
                            | Dtools.Var.Int -> "int"
                            | Dtools.Var.Float -> "float"
                            | Dtools.Var.Bool -> "bool"
                            | Dtools.Var.String -> "string"
                            | Dtools.Var.List -> "[string]")) ;
          exit 1
      | e -> print_error "Unknown error" ; raise e

let from_file ~ns filename =
  let ic = open_in filename in
    from_in_channel ~ns ic ;
    close_in ic

let parse_libs () =
  if !Configure.load_libs then
  List.iter
    (fun dir ->
       Array.iter
         (fun file ->
            if Filename.check_suffix file ".liq" then
              from_file ~ns:(Some (Filename.basename file)) (dir^"/"^file))
         (try Sys.readdir dir with _ -> [||]))
    Configure.libs_path

let from_file filename =
  parse_libs () ;
  from_file ~ns:None filename

let from_string expr =
  parse_libs () ;
  let i,o = Unix.pipe () in
  let i = Unix.in_channel_of_descr i in
  let o = Unix.out_channel_of_descr o in
    output_string o expr ;
    close_out o ;
    from_in_channel ~ns:None i ;
    close_in i

let from_in_channel x =
  parse_libs () ;
  from_in_channel ~ns:None x
