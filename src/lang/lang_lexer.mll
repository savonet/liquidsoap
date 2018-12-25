(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
{
  open Lang_parser
  open Lexing

  let incrline ?(n=1) lexbuf =
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with
          pos_bol = lexbuf.lex_curr_p.pos_cnum ;
          pos_lnum = n + lexbuf.lex_curr_p.pos_lnum }

  let parse_time t =
    let g sub n =
      let s = Pcre.get_substring sub n in
        if s="" then None else
          Some (int_of_string (String.sub s 0 (String.length s - 1)))
    in
      try
        let pat = "^((?:\\d+w)?)((?:\\d+h)?)((?:\\d+m)?)((?:\\d+s)?)$" in
        let sub = Pcre.exec ~pat t in
        let g = g sub in
          List.map g [1;2;3;4]
      with Not_found ->
        let pat = "^((?:\\d+w)?)(\\d+h)(\\d+)$" in
        let sub = Pcre.exec ~pat t in
        let g = g sub in
          [g 1;g 2;Some (int_of_string (Pcre.get_substring sub 3));None]

  (** Process multiline string syntax à la Caml (backslash-newline).
    * This is done almost in-place, mutating the initial string. *)
  let process_bytes s =
    let copy,cut =
      let pos = ref 0 in
        (fun i ->
           if !pos<>i then Bytes.set s !pos (Bytes.get  s i) ;
           incr pos),
        (fun () -> Bytes.sub s 0 !pos)
    in
    let len = Bytes.length s in
    let rec search i test =
      if i >= len then raise Not_found ;
      if test (Bytes.get s i) then i else
        search (i+1) test
    in
    let rec parse i =
      if i = len-1 then copy i else
      if i = len-2 then begin copy i ; copy (i+1) end else
        parse
          (if (Bytes.get s i) = '\\' && (Bytes.get s (i+1)) = '\n' then
             let i = search (i+2) (fun c -> c <> ' ') in
               if (Bytes.get s i) = '\\' && i+1<len &&
                  (Bytes.get s (i+1)) = ' ' then i+1 else i
           else begin
             copy i ; i+1
           end)
    in
      (try parse 0 with _ -> ()) ;
      cut ()

  let process_string s =
    Bytes.to_string
      (process_bytes (Bytes.of_string s))
}

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let var =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' ]
  ['A'-'Z' 'a'-'z' '_' '.'
     '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']*

let time =
    ( (['0'-'9']+ 'w')? (['0'-'9']+ 'h') (['0'-'9']+))
  | ( (['0'-'9']+ 'w') (['0'-'9']+ 'h')? (['0'-'9']+ 'm')? (['0'-'9']+ 's')?)
  | ( (['0'-'9']+ 'w')? (['0'-'9']+ 'h') (['0'-'9']+ 'm')? (['0'-'9']+ 's')?)
  | ( (['0'-'9']+ 'w')? (['0'-'9']+ 'h')? (['0'-'9']+ 'm') (['0'-'9']+ 's')?)
  | ( (['0'-'9']+ 'w')? (['0'-'9']+ 'h')? (['0'-'9']+ 'm')? (['0'-'9']+ 's'))

rule token = parse
  | [' ' '\t' '\r']    { token lexbuf }
  | '\n'               { incrline lexbuf ; PP_ENDL }
  | (('#' [^'\n'] * '\n') + as doc)
      { let doc = Pcre.split ~pat:"\n" doc in
          incrline ~n:(List.length doc) lexbuf ;
          PP_COMMENT doc }

  | "%ifdef"       { PP_IFDEF }
  | "%ifndef"      { PP_IFNDEF }
  | "%ifencoder"   { PP_IFENCODER }
  | "%ifnencoder"  { PP_IFNENCODER }
  | "%endif"       { PP_ENDIF }

  | "%include" [' ' '\t']* '"' ([^ '"' '>' '\n']* as file) '"'
               { PP_INCLUDE file }
  | "%include" [' ' '\t']* '<' ([^ '"' '>' '\n']* as file) '>'
               { PP_INCLUDE (Filename.concat Configure.libs_dir file) }
  | "%define"  { PP_DEFINE }

  | '#' [^'\n']* eof { EOF }
  | eof { EOF }

  | "def"    { PP_DEF }
  | "rec"    { REC }
  | "fun"    { FUN }
  | '='      { GETS }
  | "end"    { END }
  | "begin"  { BEGIN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "elsif"  { ELSIF }
  | "->"     { YIELDS }

  | "server.wait"      { SERVER_WAIT }
  | "server.write"     { SERVER_WRITE }
  | "server.read"      { SERVER_READ }
  | "server.readchars" { SERVER_READCHARS }
  | "server.readline"  { SERVER_READLINE }

  | "%ogg"    { OGG }
  | "%vorbis" { VORBIS }
  | "%opus"   { OPUS }
  | "%flac"   { FLAC }
  | "%vorbis.cbr" { VORBIS_CBR }
  | "%vorbis.abr" { VORBIS_ABR }
  | "%theora" { THEORA }
  | "%external" { EXTERNAL }
  | "%gstreamer" { GSTREAMER }
  | "%speex"  { SPEEX }
  | "%wav" { WAV }
  | "%avi" { AVI }
  | "%mp3"     { MP3 }
  | "%mp3.cbr" { MP3 }
  | "%mp3.abr" { MP3_ABR }
  | "%mp3.vbr" { MP3_VBR }
  | "%mp3.fxp" { SHINE }
  | "%shine"   { SHINE }
  | "%fdkaac" { FDKAAC }

  | '[' { LBRA }
  | ']' { RBRA }
  | '(' { LPAR }
  | ')' { RPAR }
  | '{' { LCUR }
  | '}' { RCUR }
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEQ }
  | ";;" { SEQSEQ }
  | "~" { TILD }
  | "?" { QUESTION }
  | "-" { MINUS }
  | "not" { NOT }
  | "and" | "or"                   { BIN0 (Lexing.lexeme lexbuf) }
  | "!="
  | "==" | "<" | "<=" | ">" | ">=" { BIN1 (Lexing.lexeme lexbuf) }
  | "+" | "%" | "^" | "+." | "-."  { BIN2 (Lexing.lexeme lexbuf) }
  | "/" | "*." | "/."              { BIN3 (Lexing.lexeme lexbuf) }
  | "mod"                          { BIN3 (Lexing.lexeme lexbuf) }
  | "*"                            { TIMES }

  | "ref" { REF }
  | "!"   { GET }
  | ":="  { SET }

  | "true"  { BOOL true }
  | "false" { BOOL false }
  | int_literal { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | (['0'-'9']* as ipart) '.' (['0'-'9']* as fpart)
      { let fpart =
          if fpart = "" then 0. else
            (float_of_string fpart) /.
            (10. ** (float_of_int (String.length fpart)))
        in
        let ipart = if ipart = "" then 0. else float_of_string ipart in
          FLOAT (ipart +. fpart) }

  | time as t                  { TIME (parse_time t) }
  | (time as t1) [' ' '\t' '\r']* '-' [' ' '\t' '\r']* (time as t2)
                               { INTERVAL (parse_time t1, parse_time t2) }

  | var as v                   { VAR v }

  | '\'' (([^'\''] | '\\' '\'')* as s) '\''   {
            String.iter (fun c -> if c = '\n' then incrline lexbuf) s ;
            let s = process_string s in
            STRING (Pcre.substitute ~pat:"(?<!\\\\)\\\\[0-9]{3}" ~subst:(fun m ->
                        Printf.sprintf "%c" (Char.chr (int_of_string (String.sub m 1 3)))) 
                     (Pcre.substitute ~pat:"\\\\n" ~subst:(fun _ -> "\n")
                       (Pcre.substitute ~pat:"\\\\r" ~subst:(fun _ -> "\r")
                        (Pcre.substitute ~pat:"\\\\'" ~subst:(fun _ -> "'") s)))) }
  | '"' (([^'"'] | '\\' '"')* as s) '"'   {
            String.iter (fun c -> if c = '\n' then incrline lexbuf) s ;
            let s = process_string s in
            STRING (Pcre.substitute ~pat:"(?<!\\\\)\\\\[0-9]{3}" ~subst:(fun m ->
                        Printf.sprintf "%c" (Char.chr (int_of_string (String.sub m 1 3)))) 
                     (Pcre.substitute ~pat:"\\\\n" ~subst:(fun _ -> "\n")
                       (Pcre.substitute ~pat:"\\\\r" ~subst:(fun _ -> "\r")
                         (Pcre.substitute ~pat:"\\\\\"" ~subst:(fun _ -> "\"") s)))) }
