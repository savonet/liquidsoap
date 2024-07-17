{
 open Lang
 open Lang_parser

 let line = ref 0
 let last_op = ref ""
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
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let var =
  ['A'-'Z' 'a'-'z' '_' '.'
     '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']*

rule token = parse
  | [' ' '\t' '\r']    { token lexbuf }
  | '\n'               { incr line ; token lexbuf }

  | '#' [^'\n'] * '\n' { incr line ; token lexbuf }

  | eof { EOF }

  | "let" { LET }
  | "set" { SET }
  | '='   { GETS }
  | "in"  { IN }

  | '[' { LBRA }
  | ']' { RBRA }
  | '(' { LPAR }
  | ')' { RPAR }
  | ';' { SEP }
  | ',' { COMMA }

  | "root"  { ROOT }
  | "true"  { BOOL true }
  | "false" { BOOL false }
  | int_literal { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_literal { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "~" ( var as l) ":" { LABEL l }
  | var { let id = Lexing.lexeme lexbuf in
	    if operators#is_registered id
	    then ( last_op := id ; OP id )
	    else VAR id }

  | '"' (([^'"'] | '\\' '"')* as s) '"'   {  STRING s }
