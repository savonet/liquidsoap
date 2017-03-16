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
  | "fun"    { FUN }
  | '='      { GETS }
  | "end"    { END }
  | "begin"  { BEGIN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "elsif"  { ELSIF }
  | "->"     { YIELDS }

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
  | '"'      { read_string '"' lexbuf.Lexing.lex_curr_p (Buffer.create 17) lexbuf }
  | '\''      { read_string '\'' lexbuf.Lexing.lex_curr_p (Buffer.create 17) lexbuf }
and read_string c pos buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_string c pos buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string c pos buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string c pos buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string c pos buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string c pos buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string c pos buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string c pos buf lexbuf }
  | '\\' ('\n' ('\r'|'\t'|' ')* as s)  {
      String.iter (fun c -> if c = '\n' then incrline lexbuf) s ;    
      read_string c pos buf lexbuf }
  | '\\' 'x' (((['0'-'9']|['a'-'f']|['A'-'F']) (['0'-'9']|['a'-'f']|['A'-'F'])) as code) {
      let code = int_of_string (Printf.sprintf "0x%s" code) in
      Buffer.add_char buf (Char.chr code);
      read_string c pos buf lexbuf }
  | '\\' 'o' ((('0'|'1') ('0'|'1') ('0'|'1')) as code) {
      let code = int_of_string (Printf.sprintf "0o%s" code) in
      Buffer.add_char buf (Char.chr code);
      read_string c pos buf lexbuf }
  | '\\' ((['0'-'9'] ['0'-'9'] ['0'-'9']) as code) {
      let code = int_of_string code in
      Buffer.add_char buf (Char.chr code);
      read_string c pos buf lexbuf }
  | '\\' (('"'|'\'') as c') {
      Buffer.add_char buf c';
      read_string c pos buf lexbuf }
  | '\\' (_ as c') {
      Printf.printf "Warning: illegal backslash escape in string.\n";
      Buffer.add_char buf '\\';
      Buffer.add_char buf c';
      read_string c pos buf lexbuf }
  | '\n' {
      incrline lexbuf;
      Buffer.add_char buf '\n';
      read_string c pos buf lexbuf }
  | [^ '"' '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string c pos buf lexbuf }
  | (('"'|'\'') as c') { 
      if c = c' then
         STRING (Buffer.contents buf)
      else
       begin
        Buffer.add_char buf c;
        read_string c pos buf lexbuf
       end }
  | _  { raise (Lang_values.Parse_error ((pos,lexbuf.Lexing.lex_curr_p),("Illegal string character: " ^ Lexing.lexeme lexbuf))) }
  | eof { raise (Lang_values.Parse_error ((pos,lexbuf.Lexing.lex_curr_p),("String is not terminated"))) }
