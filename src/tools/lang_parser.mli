type token =
  | VAR of (string)
  | VARLPAR of (string)
  | VARLBRA of (string)
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | TIME of (int option list)
  | INTERVAL of (int option list * int option list)
  | EOF
  | SET
  | DEF
  | BEGIN
  | END
  | GETS
  | TILD
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | COMMA
  | SEQ
  | LBRA
  | RBRA
  | LCUR
  | RCUR
  | FUN
  | YIELDS
  | BIN0 of (string)
  | BIN1 of (string)
  | BIN2 of (string)
  | BIN3 of (string)
  | MINUS
  | NOT
  | PP_IFDEF
  | PP_ENDIF
  | PP_ENDL

val scheduler :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lang.value
