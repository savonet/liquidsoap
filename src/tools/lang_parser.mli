type token =
  | ROOT
  | VAR of (string)
  | OP of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | LABEL of (string)
  | EOF
  | COMMA
  | SET
  | LET
  | GETS
  | IN
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | SEP

val scheduler :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.source
