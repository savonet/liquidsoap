type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type lexbuf = {
  lexbuf: Sedlexing.lexbuf;
  mutable lex_marked_p:   position;
  mutable lex_start_p:    position;
  mutable lex_curr_p:     position
}

val start: lexbuf -> unit
val next: lexbuf -> Uchar.t option
val mark: lexbuf -> int -> unit
val backtrack: lexbuf -> int

module Utf8 : sig
  val from_channel : in_channel -> lexbuf
  val from_interactive_channel : in_channel -> lexbuf
  val from_string : string -> lexbuf
  val lexeme: lexbuf -> string
end
