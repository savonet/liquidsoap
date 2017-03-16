type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type lexbuf = {
  lexbuf: Sedlexing.lexbuf;
  mutable lex_start_pos:  int;
  mutable lex_curr_pos:   int;
  mutable lex_marked_pos: int;
  mutable lex_marked_p:   position;
  mutable lex_start_p:    position;
  mutable lex_curr_p:     position
}

let empty_p () = {
  Lexing.
    pos_fname = "";
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0;
}

let make lexbuf = {
  lexbuf = lexbuf;
  lex_start_pos = 0;
  lex_curr_pos = 0;
  lex_marked_pos = 0;
  lex_marked_p = empty_p ();
  lex_start_p = empty_p ();
  lex_curr_p = empty_p ()
}

let new_line ?(n=1) lexbuf =
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
      Lexing.
        pos_bol = lexbuf.lex_curr_p.Lexing.pos_cnum ;
        pos_lnum = lexbuf.lex_curr_p.Lexing.pos_lnum+n }

let incr_curr_p lexbuf =
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
      Lexing.
        pos_cnum = lexbuf.lex_curr_p.Lexing.pos_cnum+1 }

let start lexbuf =
  lexbuf.lex_start_p <- lexbuf.lex_curr_p;
  lexbuf.lex_marked_p <- lexbuf.lex_curr_p;
  lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
  Sedlexing.start lexbuf.lexbuf

let next lexbuf =
  let c = Sedlexing.next lexbuf.lexbuf in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos+1;
  begin
    match c with
      (* '\n' = 10 *)
      | 10 -> new_line lexbuf
      | _ -> incr_curr_p lexbuf
  end;
  c

let mark lexbuf =
  lexbuf.lex_marked_p <- lexbuf.lex_curr_p;
  lexbuf.lex_marked_pos <- lexbuf.lex_curr_pos;
  Sedlexing.mark lexbuf.lexbuf

let backtrack lexbuf =
  lexbuf.lex_curr_p <- lexbuf.lex_marked_p;
  lexbuf.lex_curr_pos <- lexbuf.lex_marked_pos;
  Sedlexing.backtrack lexbuf.lexbuf 

module Utf8 = struct
  let from_channel ch =
    make (Sedlexing.Utf8.from_channel ch) 
  let from_string s =
    make (Sedlexing.Utf8.from_string s)
  let lexeme {lexbuf} = Sedlexing.Utf8.lexeme lexbuf
end
