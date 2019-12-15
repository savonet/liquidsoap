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

open Lang_parser

let parse_time t =
  let g sub n =
    let s = Pcre.get_substring sub n in
    if s = "" then None
    else Some (int_of_string (String.sub s 0 (String.length s - 1)))
  in
  try
    let pat = "^((?:\\d+w)?)((?:\\d+h)?)((?:\\d+m)?)((?:\\d+s)?)$" in
    let sub = Pcre.exec ~pat t in
    let g = g sub in
    List.map g [1; 2; 3; 4]
  with Not_found ->
    let pat = "^((?:\\d+w)?)(\\d+h)(\\d+)$" in
    let sub = Pcre.exec ~pat t in
    let g = g sub in
    [g 1; g 2; Some (int_of_string (Pcre.get_substring sub 3)); None]

let skipped = [%sedlex.regexp? Sub (white_space, '\n') | '\r' | '\t']
let decimal_digit = [%sedlex.regexp? '0' .. '9']

let decimal_literal =
  [%sedlex.regexp? decimal_digit, Star (decimal_digit | '_')]

let hex_literal =
  [%sedlex.regexp?
    '0', ('x' | 'X'), ascii_hex_digit, Star (ascii_hex_digit | '_')]

let oct_digit = [%sedlex.regexp? '0' .. '7']

let oct_literal =
  [%sedlex.regexp? '0', ('o' | 'O'), oct_digit, Star (oct_digit | '_')]

let bin_digit = [%sedlex.regexp? '0' | '1']

let bin_literal =
  [%sedlex.regexp? '0', ('b' | 'B'), bin_digit, Star (bin_digit | '_')]

let int_literal =
  [%sedlex.regexp? decimal_literal | hex_literal | oct_literal | bin_literal]

let var_char = [%sedlex.regexp? alphabetic | other_alphabetic]
let var_underscore = [%sedlex.regexp? '_', Plus '_']

let var_lit =
  [%sedlex.regexp?
    ( var_underscore
    | Star '_', var_char, Star (var_char | decimal_digit | '_' | '.' | '\'') )]

let var_ref =
  [%sedlex.regexp? "ref", Plus (var_char | decimal_digit | '.' | '\'')]

let var = [%sedlex.regexp? var_lit | so | math | other_math]

let time =
  [%sedlex.regexp?
    ( Opt (Plus decimal_digit, 'w'), Plus decimal_digit, 'h', Plus decimal_digit
    | ( Plus decimal_digit,
        'w',
        Opt (Plus decimal_digit, 'h'),
        Opt (Plus decimal_digit, 'm'),
        Opt (Plus decimal_digit, 's') )
    | ( Opt (Plus decimal_digit, 'w'),
        Plus decimal_digit,
        'h',
        Opt (Plus decimal_digit, 'm'),
        Opt (Plus decimal_digit, 's') )
    | ( Opt (Plus decimal_digit, 'w'),
        Opt (Plus decimal_digit, 'h'),
        Plus decimal_digit,
        'm',
        Opt (Plus decimal_digit, 's') )
    | ( Opt (Plus decimal_digit, 'w'),
        Opt (Plus decimal_digit, 'h'),
        Opt (Plus decimal_digit, 'm'),
        Plus decimal_digit,
        's' ) )]

let rec token lexbuf =
  match%sedlex lexbuf with
    | skipped -> token lexbuf
    | Plus ('#', Star (Compl '\n'), '\n') ->
        let doc = Sedlexing.Utf8.lexeme lexbuf in
        let doc = Pcre.split ~pat:"\n" doc in
        PP_COMMENT doc
    | '\n' -> PP_ENDL
    | "%ifdef" -> PP_IFDEF
    | "%ifndef" -> PP_IFNDEF
    | "%ifencoder" -> PP_IFENCODER
    | "%ifnencoder" -> PP_IFNENCODER
    | "%endif" -> PP_ENDIF
    | "%include", Star (white_space | '\t'), '"', Star (Compl '"'), '"' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '"' in
        let r = String.rindex matched '"' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        PP_INCLUDE file
    | "%include", Star (white_space | '\t'), '<', Star (Compl '>'), '>' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let n = String.index matched '<' in
        let r = String.rindex matched '>' in
        let file = String.sub matched (n + 1) (r - n - 1) in
        PP_INCLUDE (Filename.concat Configure.libs_dir file)
    | "%define" -> PP_DEFINE
    | '#', Star (Compl '\n'), eof -> EOF
    | eof -> EOF
    | "def" -> PP_DEF
    | "let" -> LET
    | "fun" -> FUN
    | "rec" -> REC
    | '=' -> GETS
    | "end" -> END
    | "begin" -> BEGIN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "elsif" -> ELSIF
    | "->" -> YIELDS
    | "server.wait" -> SERVER_WAIT
    | "server.write" -> SERVER_WRITE
    | "server.read" -> SERVER_READ
    | "server.readchars" -> SERVER_READCHARS
    | "server.readline" -> SERVER_READLINE
    | "%ogg" -> OGG
    | "%vorbis" -> VORBIS
    | "%opus" -> OPUS
    | "%flac" -> FLAC
    | "%ffmpeg" -> FFMPEG
    | "%vorbis.cbr" -> VORBIS_CBR
    | "%vorbis.abr" -> VORBIS_ABR
    | "%theora" -> THEORA
    | "%external" -> EXTERNAL
    | "%gstreamer" -> GSTREAMER
    | "%speex" -> SPEEX
    | "%wav" -> WAV
    | "%avi" -> AVI
    | "%mp3" -> MP3
    | "%mp3.cbr" -> MP3
    | "%mp3.abr" -> MP3_ABR
    | "%mp3.vbr" -> MP3_VBR
    | "%mp3.fxp" -> SHINE
    | "%shine" -> SHINE
    | "%fdkaac" -> FDKAAC
    | '[' -> LBRA
    | ']' -> RBRA
    | '(' -> LPAR
    | ')' -> RPAR
    | '{' -> LCUR
    | '}' -> RCUR
    | ',' -> COMMA
    | ':' -> COLON
    | ';' -> SEQ
    | ";;" -> SEQSEQ
    | "~" -> TILD
    | "?" -> QUESTION
    | "-" -> MINUS
    | "not" -> NOT
    | "and" | "or" -> BIN0 (Sedlexing.Utf8.lexeme lexbuf)
    | "!=" | "==" | "<" | "<=" | ">" | ">=" ->
        BIN1 (Sedlexing.Utf8.lexeme lexbuf)
    | "+" | "%" | "^" | "+." | "-." -> BIN2 (Sedlexing.Utf8.lexeme lexbuf)
    | "/" | "*." | "/." -> BIN3 (Sedlexing.Utf8.lexeme lexbuf)
    | "mod" -> BIN3 (Sedlexing.Utf8.lexeme lexbuf)
    | "*" -> TIMES
    | var_ref -> VAR (Sedlexing.Utf8.lexeme lexbuf)
    | "ref" -> REF
    | "!" -> GET
    | ":=" -> SET
    | '_' -> UNDERSCORE
    | "true" -> BOOL true
    | "false" -> BOOL false
    | int_literal -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | Star decimal_digit, '.', Star decimal_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched '.' in
        let ipart = String.sub matched 0 idx in
        let fpart =
          String.sub matched (idx + 1) (String.length matched - idx - 1)
        in
        let fpart =
          if fpart = "" then 0.
          else
            float_of_string fpart /. (10. ** float_of_int (String.length fpart))
        in
        let ipart = if ipart = "" then 0. else float_of_string ipart in
        FLOAT (ipart +. fpart)
    | time -> TIME (parse_time (Sedlexing.Utf8.lexeme lexbuf))
    | time, Star skipped, '-', Star skipped, time ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched '-' in
        let t1 = String.sub matched 0 idx in
        let t1 = String.trim t1 in
        let t2 =
          String.sub matched (idx + 1) (String.length matched - idx - 1)
        in
        let t2 = String.trim t2 in
        INTERVAL (parse_time t1, parse_time t2)
    | var -> VAR (Sedlexing.Utf8.lexeme lexbuf)
    | '"' ->
        read_string '"'
          (fst (Sedlexing.lexing_positions lexbuf))
          (Buffer.create 17) lexbuf
    | '\'' ->
        read_string '\''
          (fst (Sedlexing.lexing_positions lexbuf))
          (Buffer.create 17) lexbuf
    | _ ->
        raise
          (Lang_values.Parse_error
             ( Sedlexing.lexing_positions lexbuf,
               "Parse error: " ^ Sedlexing.Utf8.lexeme lexbuf ))

and read_string c pos buf lexbuf =
  match%sedlex lexbuf with
    | '\\', '/' ->
        Buffer.add_char buf '/';
        read_string c pos buf lexbuf
    | '\\', '\\' ->
        Buffer.add_char buf '\\';
        read_string c pos buf lexbuf
    | '\\', 'b' ->
        Buffer.add_char buf '\b';
        read_string c pos buf lexbuf
    | '\\', 'f' ->
        Buffer.add_char buf '\012';
        read_string c pos buf lexbuf
    | '\\', 'n' ->
        Buffer.add_char buf '\n';
        read_string c pos buf lexbuf
    | '\\', 'r' ->
        Buffer.add_char buf '\r';
        read_string c pos buf lexbuf
    | '\\', 't' ->
        Buffer.add_char buf '\t';
        read_string c pos buf lexbuf
    | '\\', '\n', Star skipped -> read_string c pos buf lexbuf
    | '\\', 'x', ascii_hex_digit, ascii_hex_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched 'x' in
        let code = String.sub matched (idx + 1) 2 in
        let code = int_of_string (Printf.sprintf "0x%s" code) in
        Buffer.add_char buf (Char.chr code);
        read_string c pos buf lexbuf
    | '\\', 'o', oct_digit, oct_digit, oct_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let idx = String.index matched 'o' in
        let code = String.sub matched (idx + 1) 3 in
        let code = int_of_string (Printf.sprintf "0o%s" code) in
        Buffer.add_char buf (Char.chr code);
        read_string c pos buf lexbuf
    | '\\', decimal_digit, decimal_digit, decimal_digit ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let code = String.sub matched 1 3 in
        let code = int_of_string code in
        Buffer.add_char buf (Char.chr code);
        read_string c pos buf lexbuf
    | '\\', ('"' | '\'') ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        Buffer.add_char buf matched.[1];
        read_string c pos buf lexbuf
    | '\\', any ->
        Printf.printf "Warning: illegal backslash escape in string.\n";
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_string c pos buf lexbuf
    | Plus (Compl ('"' | '\'' | '\\')) ->
        Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        read_string c pos buf lexbuf
    | '"' | '\'' ->
        let matched = Sedlexing.Utf8.lexeme lexbuf in
        let c' = matched.[0] in
        if c = c' then STRING (Buffer.contents buf)
        else (
          Buffer.add_char buf c';
          read_string c pos buf lexbuf )
    | eof ->
        raise
          (Lang_values.Parse_error
             ( (pos, snd (Sedlexing.lexing_positions lexbuf)),
               "String is not terminated" ))
    | _ ->
        raise
          (Lang_values.Parse_error
             ( (pos, snd (Sedlexing.lexing_positions lexbuf)),
               "Illegal string character: " ^ Sedlexing.Utf8.lexeme lexbuf ))
