{
(*  open Scheduling_lexer *)
  open Scheduling_parser
}

rule token = parse
  | [' ' '\t' '\r' '\n']   { token lexbuf }     (* skip blanks *)
  | ['0'-'9']+        { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '-'               { INTER }
  | "&&" | '&'        { AND }
  | "||" | '|'        { OR }
  | "(" | "["         { LEFT }
  | ")" | "]"         { RIGHT }
  | 'w' | 'd'         { WDAY }
  | 'h'               { HOUR }
  | "m"               { MIN }
  | 's'               { SEC }
  | eof               { EOF }
