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

let empty_p () = {
  Lexing.
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
}

let make lexbuf = {
  lexbuf = lexbuf;
  lex_marked_p = empty_p ();
  lex_start_p = empty_p ();
  lex_curr_p = empty_p ()
}

let new_line lexbuf =
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
      Lexing.
        pos_bol = lexbuf.lex_curr_p.Lexing.pos_cnum ;
        pos_lnum = lexbuf.lex_curr_p.Lexing.pos_lnum+1}

let incr_curr lexbuf =
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
      Lexing.
        pos_cnum = lexbuf.lex_curr_p.Lexing.pos_cnum+1 }

let start lexbuf =
  lexbuf.lex_start_p <- lexbuf.lex_curr_p;
  lexbuf.lex_marked_p <- lexbuf.lex_curr_p;
  Sedlexing.start lexbuf.lexbuf

let next lexbuf =
  let c = Sedlexing.next lexbuf.lexbuf in
  incr_curr lexbuf;
  (* '\n' = 10 *)
  if c = Some (Uchar.of_char '\n') then new_line lexbuf;
  c

let mark lexbuf =
  lexbuf.lex_marked_p <- lexbuf.lex_curr_p;
  Sedlexing.mark lexbuf.lexbuf

let backtrack lexbuf =
  lexbuf.lex_curr_p <- lexbuf.lex_marked_p;
  Sedlexing.backtrack lexbuf.lexbuf 

module Utf8 = struct
  let from_channel ch =
    let stream = Stream.of_channel ch in
    let gen () =
      try Some (Stream.next stream) with Stream.Failure -> None
    in
    make (Sedlexing.Utf8.from_gen gen)

  (* See ocaml-community/sedlex#45 *)
  let from_interactive_channel ch =
    let chunk_size = 512 in
    let buf = Bytes.create chunk_size in
    let cached = ref (-1) in
    let position = ref (-1) in
    let rec gen () =
      match !position, !cached  with
        | _, 0 ->
            None
        | -1, _ ->
            begin
              position := 0;
              cached := input ch buf 0 chunk_size
            end;
            gen ()
        | len, c when len = c ->
            position := -1;
            (* This means that the last read was a full
             * chunk. Safe to try a new one right away. *)
            if len = chunk_size then
              gen ()
            else
              None
        | len, _ ->
            position := len+1;
            Some (Bytes.get buf len)
    in
    make (Sedlexing.Utf8.from_gen gen)

  let from_string s =
    make (Sedlexing.Utf8.from_string s)

  let lexeme {lexbuf} = Sedlexing.Utf8.lexeme lexbuf
end
