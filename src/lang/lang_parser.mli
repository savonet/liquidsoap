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
  | BEGIN
  | END
  | GETS
  | TILD
  | DEF of (Doc.item * (string*string) list)
  | IF
  | THEN
  | ELSE
  | ELSIF
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
  | PP_INCLUDE
  | PP_DEF
  | PP_COMMENT of (string list)

val scheduler :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lang_values.value
