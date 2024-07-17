type token =
  | INT of (int)
  | EOF
  | SEC
  | MIN
  | HOUR
  | DAY
  | WDAY
  | LPAR
  | RPAR
  | AND
  | OR
  | INTER
  | LEFT
  | RIGHT

val schedule :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Scheduling_defs.expr
