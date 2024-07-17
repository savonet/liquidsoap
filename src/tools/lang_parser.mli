type token =
  | VAR of (string)
  | VARLPAR of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | TIME of (string list)
  | EOF
  | SET
  | DEF
  | BEGIN
  | END
  | GETS
  | TILD
  | LPAR
  | RPAR
  | COMMA
  | SEQ
  | LBRA
  | RBRA
  | LCUR
  | RCUR
  | FUNLPAR
  | YIELDS
  | INTERP
  | CONCAT
  | MINUS
  | AND
  | OR

val scheduler :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lang.value
