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
{
  open Lang
  open Lang_parser
  open Lexing

  let incrline lexbuf =
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with
          pos_bol = lexbuf.lex_curr_p.pos_cnum ;
          pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum }

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

rule token = parse
  | [' ' '\t' '\r']    { token lexbuf }
  | '\n'               { incrline lexbuf ; token lexbuf }
  | '#' [^'\n'] * '\n' { incrline lexbuf ; token lexbuf }

  | eof { EOF }

  | "set"    { SET }
  | '='      { GETS }
  | "def"    { DEF }
  | "end"    { END }
  | "begin"  { BEGIN }
  | "->"     { YIELDS }

  | '[' { LBRA }
  | ']' { RBRA }
  | '(' { LPAR }
  | ')' { RPAR }
  | '{' { LCUR }
  | '}' { RCUR }
  | ',' { COMMA }
  | ';' { SEQ }
  | "~" { TILD }
  | "^" { CONCAT }
  | "-" { MINUS }

  | "and" { AND }
  | "or"  { OR }

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

  | "fun" [' ''\t']* '('       { FUNLPAR }
  | (var as v) [' ''\t']* '('  { VARLPAR v }

  | ( (['0'-'9']+ 'w')? as w)
    ( (['0'-'9']+ 'h')  as h)
    ( (['0'-'9']+    )  as m)  { TIME [w;h;m^"m";""] }
  | ( (['0'-'9']+ 'w')? as w)
    ( (['0'-'9']+ 'h')? as h)
    ( (['0'-'9']+ 'm')? as m)
    ( (['0'-'9']+ 's')? as s)  { TIME [w;h;m;s] }

  | var                        { VAR (Lexing.lexeme lexbuf) }

  | '\'' (([^'\''] | '\\' '\'')* as s) '\''   {
            String.iter (fun c -> if c = '\n' then incrline lexbuf) s ;
            STRING (Str.global_replace (Str.regexp "\\\\'") "'" s) }
  | '"' (([^'"'] | '\\' '"')* as s) '"'   {
            String.iter (fun c -> if c = '\n' then incrline lexbuf) s ;
            STRING (Str.global_replace (Str.regexp "\\\\\"") "\"" s) }
