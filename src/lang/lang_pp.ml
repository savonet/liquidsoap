(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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

type tokenizer = unit -> Lang_parser.token*Sedlexing_compat.position*Sedlexing_compat.position

let fst (x,_,_) = x
let snd (_,y,_) = y
let trd (_,_,z) = z

let mk_tokenizer ?(fname="") lexbuf () =
  lexbuf.Sedlexing_compat.lex_start_p <-
    { lexbuf.Sedlexing_compat.lex_start_p with
        Sedlexing_compat.pos_fname = fname } ;
  lexbuf.Sedlexing_compat.lex_curr_p <-
    { lexbuf.Sedlexing_compat.lex_curr_p with
        Sedlexing_compat.pos_fname = fname } ;
  (Lang_lexer.token lexbuf,
   lexbuf.Sedlexing_compat.lex_start_p,
   lexbuf.Sedlexing_compat.lex_curr_p)

(* TODO: also parse optional arguments? *)
let get_encoder_format tokenizer =
  let ogg_item = function
    | Lang_parser.VORBIS -> Lang_encoders.vorbis []
    | Lang_parser.VORBIS_CBR -> Lang_encoders.vorbis_cbr []
    | Lang_parser.VORBIS_ABR -> Lang_encoders.vorbis_abr []
    | Lang_parser.THEORA -> Lang_encoders.theora []
    | Lang_parser.SPEEX -> Lang_encoders.speex []
    | Lang_parser.OPUS -> Lang_encoders.opus []
    | Lang_parser.FLAC -> Lang_encoders.ogg_flac []
    | _ -> failwith "ogg format expected"
  in
  let is_ogg_item token =
    try let _ = ogg_item token in true with _ -> false
  in
    match tokenizer() with
    | Lang_parser.MP3,_,_ -> Lang_encoders.mp3_cbr []
    | Lang_parser.MP3_VBR,_,_ -> Lang_encoders.mp3_vbr []
    | Lang_parser.MP3_ABR,_,_ -> Lang_encoders.mp3_vbr []
    | Lang_parser.SHINE,_,_   -> Lang_encoders.shine []
    | Lang_parser.FDKAAC,_,_ -> Lang_encoders.fdkaac []
    | Lang_parser.FLAC,_,_ -> Lang_encoders.flac []
    | Lang_parser.EXTERNAL,_,_ -> Lang_encoders.external_encoder []
    | Lang_parser.GSTREAMER,_,_ -> Lang_encoders.gstreamer []
    | Lang_parser.WAV,_,_ -> Lang_encoders.wav []
    | ogg,_,_ when is_ogg_item ogg ->
      let ogg = ogg_item ogg in
        Encoder.Ogg [ogg]
    (* TODO *)
    (* | Lang_parser.OGG -> Lang_encoders.mk ... [] *)
    | _ -> failwith "expected an encoding format after %ifencoder"

(* The Lang_lexer is not quite enough for our needs,
 * so we first define convenient layers between it and the parser.
 * First a pre-processor which evaluates %ifdefs. *)
let eval_ifdefs tokenizer =
  let state = ref 0 in
  let rec token() =
    let go_on () =
      incr state ;
      token()
    in
    let rec skip () =
      match tokenizer() with
      | Lang_parser.PP_ENDIF,_,_ -> token()
      | _ -> skip ()
    in
    match tokenizer() with
      | Lang_parser.PP_IFDEF,_,_ | Lang_parser.PP_IFNDEF,_,_ as tok ->
          begin match tokenizer() with
            | Lang_parser.VAR v,_,_ ->
                let test =
                  if fst(tok) = Lang_parser.PP_IFDEF then fun x -> x else not
                in
                (** XXX Less natural meaning than the original one. *)
                if test (Lang_values.builtins#is_registered v) then
                  go_on ()
                else
                  skip ()
            | _ -> failwith "expected a variable after %ifdef"
          end
      | Lang_parser.PP_IFENCODER,_,_ | Lang_parser.PP_IFNENCODER,_,_ as tok ->
          let fmt = get_encoder_format tokenizer in
          let has_enc =
            try let _ = Encoder.get_factory fmt in true with Not_found -> false
          in
          let test =
            if fst(tok) = Lang_parser.PP_IFENCODER then fun x -> x else not
          in
            if test has_enc then go_on () else skip ()
      | Lang_parser.PP_ENDIF,_,_ ->
          if !state=0 then failwith "no %ifdef to end here" ;
          decr state ;
          token()
      | x -> x
  in
    token

(** Expand %include statements by inserting the content of files.
  * Filenames are understood relatively to the current directory,
  * which can be a relative path such as "." or "..". *)
let includer dir tokenizer =
  let stack = Stack.create () in
  let peek () =
    try
      fst(Stack.top stack)
    with Stack.Empty -> tokenizer
  in
  let current_dir () =
    try
      snd(Stack.top stack)
    with Stack.Empty -> dir
  in
  let rec token () =
    match peek () () with
      | Lang_parser.PP_INCLUDE fname,startp,_ ->
          let fname = Utils.home_unrelate fname in
          let fname =
            if Filename.is_relative fname then
              Filename.concat (current_dir ()) fname
            else
              fname
          in
          let channel =
            try open_in fname with
              | Sys_error _ ->
                  flush_all () ;
                    Printf.printf "%sine %d, char %d: cannot %%include, "
                      (if startp.Sedlexing_compat.pos_fname="" then "L" else
                         Printf.sprintf "File %S, l" startp.Sedlexing_compat.pos_fname)
                      startp.Sedlexing_compat.pos_lnum
                      (1+startp.Sedlexing_compat.pos_cnum-startp.Sedlexing_compat.pos_bol) ;
                    Printf.printf "file %S doesn't exist.\n" fname ;
                    exit 1
          in
          let lexbuf = Sedlexing_compat.Utf8.from_channel channel in
          let tokenizer = mk_tokenizer ~fname lexbuf in
          Stack.push (tokenizer,Filename.dirname fname,channel) stack; 
          token()
      | Lang_parser.EOF,_,_ as tok ->
          if Stack.is_empty stack then
            tok
          else
            begin
              close_in (trd (Stack.pop stack));
              token()
            end
      | x -> x
  in
    token

(* The expander turns "bla #{e} bli" into ("bla "^string_of(e)^" bli") *)
type exp_item =
  | String of string | Expr of tokenizer | Concat | RPar | LPar | String_of
let expand_string tokenizer =
  let state = Queue.create () in
  let add startp endp x =
    Queue.add (x,startp,endp) state
  in
  let pop () = ignore (Queue.take state) in
  let parse s startp endp =
    let l = Pcre.split ~pat:"#{(.*?)}" s in
    let l = if l = [] then [""] else l in
    let add = add startp endp in
    let rec parse = function
      | s::x::l ->
          let lexbuf =
            Sedlexing_compat.Utf8.from_string x
          in
          let tokenizer =
            mk_tokenizer lexbuf
          in
          let tokenizer () =
            (fst (tokenizer()),startp,endp)
          in
          List.iter add
            [ String s ; Concat; LPar ;
              String_of ; Expr tokenizer ; RPar ;
              RPar ] ;
          if l<>[] then begin
            add Concat ;
            parse l
          end
      | [x] -> add (String x);
      | [] -> assert false
    in
      parse l
  in
  let rec token () =
    if Queue.is_empty state then begin
      match tokenizer() with
        | Lang_parser.STRING s,startp,endp ->
            parse s startp endp ;
            if Queue.length state > 1 then begin
              add startp endp RPar ;
              Lang_parser.LPAR,startp,endp
            end else
              token()
        | x -> x
    end else
      let (el,startp,endp) = Queue.peek state in
      match el with
        | String s  -> pop () ; Lang_parser.STRING s, startp, endp
        | Concat    -> pop () ; Lang_parser.BIN2 "^", startp, endp
        | RPar      -> pop () ; Lang_parser.RPAR, startp, endp
        | LPar      -> pop () ; Lang_parser.LPAR, startp, endp
        | String_of -> pop () ; Lang_parser.VARLPAR "string_of", startp, endp
        | Expr tokenizer ->
            begin match tokenizer() with
              | Lang_parser.EOF,_,_ -> pop () ; token()
              | x,_,_ -> x,startp,endp
            end
  in
    token

(* Glue the documenting comments to the corresponding PP_DEF (read pre-DEF)
 * and strip out other comments. *)

let parse_comments tokenizer =
  let documented_def (doc,doc_startp) startp endp =
    let startp =
      match doc_startp with
        | Some startp -> startp
        | None -> startp
    in
    let doc =
      List.map
        (fun x -> Pcre.substitute ~pat:"^\\s*#\\s?" ~subst:(fun _ -> "") x)
        doc
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
      Lang_parser.DEF (doc,params),startp,endp
  in
  let comment = ref ([],None) in
  let rec token () =
    match tokenizer () with
      | Lang_parser.PP_COMMENT c,startp,_ -> comment := (c,Some startp) ; token()
      | Lang_parser.PP_DEF,startp,endp ->
          let c = !comment in
            comment := ([],None) ;
            documented_def c startp endp
      | x -> comment := ([],None) ; x
  in
    token

(* Last but not least: remove new lines and merge some tokens around them
 * in order to remove some ambiguities, typically between:
 *   def foo \n (x,y) ... << Normal definition, starting with a couple
 *   def foo(x,y) ...     << Definition of the function foo *)
let strip_newlines tokenizer =
  let state = ref None in
  let rec token () =
    match !state with
      | None ->
          begin match tokenizer () with
            | Lang_parser.PP_ENDL,_,_ -> token()
            | (Lang_parser.VAR _,_,_) as v ->
                state := Some v ;
                token()
            | x -> x
          end
      | Some ((Lang_parser.VAR var,startp,_) as v) ->
          begin match tokenizer() with
            | Lang_parser.LPAR,_,endp ->
                state := None ;
                Lang_parser.VARLPAR var,startp,endp
            | Lang_parser.LBRA,_,endp ->
                state := None ;
                Lang_parser.VARLBRA var,startp,endp
            | Lang_parser.PP_ENDL,_,_ ->
                state := None ; v
            | x -> state := Some x ; v
          end
      | Some x -> state := None ; x
  in
    token

(* Inline %define x value. *)
let expand_define tokenizer =
  let defs = ref [] in
  let rec token () =
    match tokenizer () with
    | Lang_parser.PP_DEFINE,startp,endp ->
      (
        match tokenizer() with
        | Lang_parser.VAR def_name,_,_ ->
          if def_name <> Utils.StringCompat.uppercase_ascii def_name then raise Parsing.Parse_error;
          (
            match tokenizer () with
            | Lang_parser.INT _,_,_
            | Lang_parser.FLOAT _,_,_
            | Lang_parser.STRING _,_,_
            | Lang_parser.BOOL _,_,_ as def_val ->
              defs := (def_name,(fst(def_val),startp,endp)) :: !defs;
              token()
            | _ -> raise Parsing.Parse_error
          )
        | _ -> raise Parsing.Parse_error
      )
    | Lang_parser.VAR def_name,_,_ as v ->
      (try List.assoc def_name !defs with Not_found -> v)
    | x -> x
  in
  token

(* Wrap the lexer with its extensions *)
let mk_tokenizer ~pwd lexbuf =
  let (=>) a b = b a in
    mk_tokenizer lexbuf
    => includer pwd
    => eval_ifdefs
    => parse_comments
    => expand_string
    => strip_newlines
    => expand_define
