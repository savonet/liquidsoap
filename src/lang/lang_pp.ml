(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
                (** XXX Less natural meaning than the original one. *)
                if Lang_values.builtins#is_registered v then begin
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

let includer tokenizer =
  (* The purpose of this preprocessor is to insert some files instead
   * of %include statements. This is done by keeping a stack ([state])
   * of the lexbufs of the nested included files.
   * In order to maintain the lexing positions, we have to copy data from
   * the current active lexbuf to the toplevel one, the only visible one
   * from the parser. *)
  let state = Stack.create () in
  let in_channels = Stack.create () in
  let copy_lexbuf dst src =
    (* Yuk. *)
    dst.Lexing.lex_buffer      <- src.Lexing.lex_buffer ;
    dst.Lexing.lex_buffer_len  <- src.Lexing.lex_buffer_len ;
    dst.Lexing.lex_abs_pos     <- src.Lexing.lex_abs_pos ;
    dst.Lexing.lex_start_pos   <- src.Lexing.lex_start_pos ;
    dst.Lexing.lex_curr_pos    <- src.Lexing.lex_curr_pos ;
    dst.Lexing.lex_last_pos    <- src.Lexing.lex_last_pos ;
    dst.Lexing.lex_last_action <- src.Lexing.lex_last_action ;
    dst.Lexing.lex_eof_reached <- src.Lexing.lex_eof_reached ;
    dst.Lexing.lex_mem         <- src.Lexing.lex_mem ;
    dst.Lexing.lex_start_p     <- src.Lexing.lex_start_p ;
    dst.Lexing.lex_curr_p      <- src.Lexing.lex_curr_p ;
  in
  let rec token top_lexbuf =
    (* At the first call the state will be initialized with a copy
     * of the toplevel buffer. This copy does not share the mutable fields.
     * Note that the copy is not a valid lexbuf, in that its filling
     * function actually modifies the fields of the original top_lexbuf.
     *
     * In other calls, state will never be empty.
     * When the state has only one layer, top_lexbuf's values are stored
     * in it, in order to be able to restore after a possible inclusion.
     * When the state has more than one layer, top_buffer is only
     * used to communicate locations to the parser. And lexing will actually
     * be done using the values in the queue. *)
    if Stack.is_empty state then
      Stack.push
        { top_lexbuf with Lexing.lex_buffer = top_lexbuf.Lexing.lex_buffer }
        state ;
    let tokenizer () =
      if Stack.length state = 1 then
        (* Save the top_lexbuf's mutable values on the stack,
         * in order to restore after a possible inclusion. *)
        let token = tokenizer top_lexbuf in
          copy_lexbuf (Stack.top state) top_lexbuf ;
          token
      else
        (* Copy the current lexbuf's info to top_lexbuf for the parser. *)
        let lexbuf = Stack.top state in
        let token = tokenizer lexbuf in
          copy_lexbuf top_lexbuf lexbuf ;
          token
    in
      match tokenizer () with
        | Lang_parser.PP_INCLUDE ->
            let new_lexbuf =
              let filename =
                match tokenizer () with
                  | Lang_parser.STRING s -> s
                  | _ -> failwith "expected a string after %include"
              in
              let channel = open_in (Utils.home_unrelate filename) in
              let new_lexbuf = Lexing.from_channel channel in
                Stack.push channel in_channels ;
                new_lexbuf.Lexing.lex_start_p <-
                  { new_lexbuf.Lexing.lex_start_p with
                      Lexing.pos_fname = filename } ;
                new_lexbuf.Lexing.lex_curr_p <-
                  { new_lexbuf.Lexing.lex_curr_p with
                      Lexing.pos_fname = filename } ;
                new_lexbuf
            in
              Stack.push new_lexbuf state ;
              token top_lexbuf
        | Lang_parser.EOF ->
            if Stack.length state = 1 then Lang_parser.EOF else begin
              ignore (Stack.pop state) ;
              close_in (Stack.pop in_channels) ;
              if Stack.length state = 1 then
                (* top_lexbuf is containing outdated data *)
                copy_lexbuf top_lexbuf (Stack.top state) ;
              token top_lexbuf
            end
        | x -> x
  in
    token

(* The expander turns "bla #{e} bli" into ("bla "^string_of(e)^" bli") *)
type exp_item =
  | String of string | Expr of Lexing.lexbuf | Concat | RPar | LPar | String_of
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
            [ String s ; Concat; LPar ;
              String_of ; Expr (Lexing.from_string x) ; RPar ;
              RPar ] ;
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
        | String s  -> pop () ; Lang_parser.STRING s
        | Concat    -> pop () ; Lang_parser.BIN2 "^"
        | RPar      -> pop () ; Lang_parser.RPAR
        | LPar      -> pop () ; Lang_parser.LPAR
        | String_of -> pop () ; Lang_parser.VARLPAR "string_of"
        | Expr b ->
            begin match tokenizer b with
              | Lang_parser.EOF -> pop () ; token lexbuf
              | x -> x
            end
  in
    token

(* Glue the documenting comments to the corresponding PP_DEF (read pre-DEF)
 * and strip out other comments. *)

let parse_comments tokenizer =
  let documented_def doc =
    (* TODO multi-line @param comments *)
    let doc =
      List.map (Pcre.substitute ~pat:"^\\s*#\\s?" ~subst:(fun _ -> "")) doc
    in
    let main,special,params =
      List.fold_left
        (fun (main,special,params) line ->
           try
             let sub =
               Pcre.exec ~pat:"^\\s*@(category|flag|param)\\s*(.*)$" line
             in
             let s = Pcre.get_substring sub 2 in
               match Pcre.get_substring sub 1 with
                 | "category" -> main, `Category s :: special, params
                 | "flag" -> main, `Flag s :: special, params
                 | "param" ->
                     let sub =
                       Pcre.exec ~pat:"^(~?[a-zA-Z0-9_.]+)\\s*(.*)$" s
                     in
                     let label = Pcre.get_substring sub 1 in
                     let descr = Pcre.get_substring sub 2 in
                     let label =
                       if label.[0] = '~' then
                         String.sub label 1 (String.length label - 1)
                       else
                         ""
                     in
                       main, special,
                       (label,descr) :: params
                 | _ -> raise Not_found
           with
             | Not_found -> line::main,special,params)
        ([],[],[])
        doc
    in
    let main = List.rev main and params = List.rev params in
    let main = String.concat "\n" main in
    let doc =
      let sort = false in
        if main = "" then Doc.none ~sort () else Doc.trivial ~sort main
    in
      List.iter
        (function
           | `Category c -> doc#add_subsection "category" (Doc.trivial c)
           | `Flag c -> doc#add_subsection "flag" (Doc.trivial c))
        special ;
      Lang_parser.DEF (doc,params)
  in
  let comment = ref [] in
  let rec token lexbuf =
    match tokenizer lexbuf with
      | Lang_parser.PP_COMMENT c -> comment := c ; token lexbuf
      | Lang_parser.PP_DEF ->
          let c = !comment in
            comment := [] ;
            documented_def c
      | x -> comment := [] ; x
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
let token =
  let (+) a b = b a in
    Lang_lexer.token
    + parse_comments
    + strip_newlines
    + preprocess
    + expand
    (* The includer has to be the last, since it uses its input tokenizer
     * (which wouldn't get extended by further additions) on inclusions. *)
    + includer
