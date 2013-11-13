(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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

(* TODO: also parse optional arguments? *)
let get_encoder_format tokenizer lexbuf =
  let ogg_item = function
    | Lang_parser.VORBIS -> Lang_encoders.vorbis []
    | Lang_parser.VORBIS_CBR -> Lang_encoders.vorbis_cbr []
    | Lang_parser.VORBIS_ABR -> Lang_encoders.vorbis_abr []
    | Lang_parser.THEORA -> Lang_encoders.theora []
    | Lang_parser.DIRAC -> Lang_encoders.dirac []
    | Lang_parser.SPEEX -> Lang_encoders.speex []
    | Lang_parser.OPUS -> Lang_encoders.opus []
    | Lang_parser.FLAC -> Lang_encoders.ogg_flac []
    | _ -> failwith "ogg format expected"
  in
  let is_ogg_item token =
    try let _ = ogg_item token in true with _ -> false
  in
    match tokenizer lexbuf with
    | Lang_parser.MP3 -> Lang_encoders.mp3_cbr []
    | Lang_parser.MP3_VBR -> Lang_encoders.mp3_vbr []
    | Lang_parser.MP3_ABR -> Lang_encoders.mp3_vbr []
    | Lang_parser.SHINE   -> Lang_encoders.shine []
    | Lang_parser.AACPLUS -> Lang_encoders.aacplus []
    | Lang_parser.VOAACENC -> Lang_encoders.voaacenc []
    | Lang_parser.FDKAAC -> Lang_encoders.fdkaac []
    | Lang_parser.FLAC -> Lang_encoders.flac []
    | Lang_parser.EXTERNAL -> Lang_encoders.external_encoder []
    | Lang_parser.GSTREAMER -> Lang_encoders.gstreamer []
    | Lang_parser.WAV -> Lang_encoders.wav []
    | ogg when is_ogg_item ogg ->
      let ogg = ogg_item ogg in
        Encoder.Ogg [ogg]
    (* TODO *)
    (* | Lang_parser.OGG -> Lang_encoders.mk ... [] *)
    | _ -> failwith "expected an encoding format after %ifencoder"

(* The Lang_lexer is not quite enough for our needs,
 * so we first define convenient layers between it and the parser.
 * First a pre-processor which evaluates %ifdefs. *)
let preprocess tokenizer =
  let state = ref 0 in
  let rec token lexbuf =
    let go_on () =
      incr state ;
      token lexbuf
    in
    let rec skip () =
      match tokenizer lexbuf with
      | Lang_parser.PP_ENDIF -> token lexbuf
      | _ -> skip ()
    in
    match tokenizer lexbuf with
      | Lang_parser.PP_IFDEF | Lang_parser.PP_IFNDEF as tok ->
          begin match tokenizer lexbuf with
            | Lang_parser.VAR v ->
                let test =
                  if tok = Lang_parser.PP_IFDEF then fun x -> x else not
                in
                (** XXX Less natural meaning than the original one. *)
                if test (Lang_values.builtins#is_registered v) then
                  go_on ()
                else
                  skip ()
            | _ -> failwith "expected a variable after %ifdef"
          end
      | Lang_parser.PP_IFENCODER | Lang_parser.PP_IFNENCODER as tok ->
          let fmt = get_encoder_format tokenizer lexbuf in
          let has_enc =
            try let _ = Encoder.get_factory fmt in true with Not_found -> false
          in
          let test =
            if tok = Lang_parser.PP_IFENCODER then fun x -> x else not
          in
            if test has_enc then go_on () else skip ()
      | Lang_parser.PP_ENDIF ->
          if !state=0 then failwith "no %ifdef to end here" ;
          decr state ;
          token lexbuf
      | x -> x
  in
    token

(** Expand %include statements by inserting the content of files.
  * Filenames are understood relatively to the current directory,
  * which can be a relative path such as "." or "..". *)
let includer dir tokenizer =
  (* We maintain a stack ([state]) of the lexbufs of the nested included files
   * and a stack of dirnames and in_channels for [opened] files.
   * In order to maintain the lexing positions, we have to copy data from
   * the current active lexbuf to the toplevel one, the only visible one
   * from the parser. *)
  let state = Stack.create () in
  let opened = Stack.create () in
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
  let current_dir () =
    if Stack.is_empty opened then dir else fst (Stack.top opened)
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
        | Lang_parser.PP_INCLUDE filename ->
            let lexbuf = Stack.top state in
            let new_lexbuf =
              let filename = Utils.home_unrelate filename in
              let filename =
                if Filename.is_relative filename then
                  Filename.concat (current_dir ()) filename
                else
                  filename
              in
              let channel =
                try open_in filename with
                  | Sys_error _ ->
                      flush_all () ;
                      let start = lexbuf.Lexing.lex_curr_p in
                        Printf.printf "%sine %d, char %d: cannot %%include, "
                          (if start.Lexing.pos_fname="" then "L" else
                             Printf.sprintf "File %S, l" start.Lexing.pos_fname)
                          start.Lexing.pos_lnum
                          (1+start.Lexing.pos_cnum-start.Lexing.pos_bol) ;
                        Printf.printf "file %S doesn't exist.\n" filename ;
                        exit 1
              in
              let new_lexbuf = Lexing.from_channel channel in
                Stack.push (Filename.dirname filename,channel) opened ;
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
              close_in (snd (Stack.pop opened)) ;
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
    let doc =
      List.map (Pcre.substitute ~pat:"^\\s*#\\s?" ~subst:(fun _ -> "")) doc
    in
    let rec parse_doc (main,special,params) = function
      | [] -> (main,special,params)
      | line::lines ->
          begin try
            let sub =
              Pcre.exec ~pat:"^\\s*@(category|flag|param)\\s*(.*)$" line
            in
            let s = Pcre.get_substring sub 2 in
              match Pcre.get_substring sub 1 with
                | "category" ->
                    parse_doc (main, `Category s :: special, params) lines
                | "flag" ->
                    parse_doc (main, `Flag s :: special, params) lines
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
                    let rec parse_descr descr lines =
                      match lines with
                        | [] -> raise Not_found
                        | line::lines ->
                            let line =
                              Pcre.substitute
                                ~pat:"^ *" ~subst:(fun _ -> "") line
                            in
                            let n = String.length line - 1 in
                              if line.[n] = '\\' then
                                let descr = String.sub line 0 n :: descr in
                                  parse_descr descr lines
                              else
                                let descr = List.rev (line::descr) in
                                  String.concat "" descr, lines
                    in
                    let descr,lines = parse_descr [] (descr::lines) in
                      parse_doc (main, special, (label,descr) :: params) lines
                | _ -> assert false
          with
            | Not_found ->
                parse_doc (line::main,special,params) lines
          end
    in
    let main,special,params = parse_doc ([],[],[]) doc in
    let main = List.rev main and params = List.rev params in
    let rec smart_concat = function
      | [] -> ""
      | [line] -> line
      | line::lines ->
          if line = "" || line.[String.length line - 1] = '.' then
            line ^ "\n" ^ smart_concat lines
          else if line.[String.length line - 1] = ' ' then
            line ^ smart_concat lines
          else
            line ^ " " ^ smart_concat lines
    in
    let main = smart_concat main in
    let doc =
      let sort = false in
        if main = "" then Doc.none ~sort () else Doc.trivial ~sort main
    in
      List.iter
        (function
           | `Category c -> doc#add_subsection "_category" (Doc.trivial c)
           | `Flag c -> doc#add_subsection "_flag" (Doc.trivial c))
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

(* Inline %define x value. *)
let expand_define tokenizer =
  let defs = ref [] in
  let rec token lexbuf =
    match tokenizer lexbuf with
    | Lang_parser.PP_DEFINE ->
      (
        match tokenizer lexbuf with
        | Lang_parser.VAR x ->
          if x <> String.uppercase x then raise Parsing.Parse_error;
          (
            match tokenizer lexbuf with
            | Lang_parser.INT _
            | Lang_parser.FLOAT _
            | Lang_parser.STRING _
            | Lang_parser.BOOL _ as v ->
              defs := (x,v) :: !defs;
              token lexbuf
            | _ -> raise Parsing.Parse_error
          )
        | _ -> raise Parsing.Parse_error
      )
    | Lang_parser.VAR v as x ->
      (try List.assoc v !defs with Not_found -> x)
    | x -> x
  in
  token

(* Wrap the lexer with its extensions *)
let token dir =
  let (+) a b = b a in
    Lang_lexer.token
    + parse_comments
    + strip_newlines
    + preprocess
    + expand
    + expand_define
    (* The includer has to be the last, since it uses its input tokenizer
     * (which wouldn't get extended by further additions) on inclusions. *)
    + includer dir
