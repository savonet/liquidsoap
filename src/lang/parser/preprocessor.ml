(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

type tokenizer = unit -> Parser.token * Term.parsed_pos

(* A string literal is read one chunk at a time, and an interpolation is read
   as ordinary tokens, so that `"#{ r.{a = 1}.a }"` and `"#{ m["k"] }"` mean
   what they look like: the `}` that closes an interpolation is found by
   counting braces over *tokens*, where a nested string, comment or raw string
   is already a single token and cannot be miscounted.

   `"a #{e} b"` yields:
     BEGIN_INTERPOLATION '"', INTERPOLATED_STRING "a ", <tokens of e>,
     INTERPOLATED_STRING " b", END_INTERPOLATION *)

(* One open interpolated string. [depth] counts the `{` currently open inside
   the interpolation being read. *)
type interpolation = { sep : char; mutable depth : int }

let mk_tokenizer ?(fname = "") lexbuf =
  Sedlexing.set_filename lexbuf fname;
  let pending = Queue.create () in
  (* Interpolated strings currently being read, innermost first. *)
  let open_strings = ref [] in
  (* Whether the next thing to read is a chunk of the innermost string rather
     than a token. *)
  let in_string = ref false in
  let positions () = Sedlexing.lexing_bytes_positions lexbuf in
  let read_chunk sep =
    let startp, _ = positions () in
    Lexer.read_string_chunk sep startp (Buffer.create 17) lexbuf
  in
  (* An empty chunk carries nothing: `"#{a}#{b}"` has no literal text between
     the two interpolations, and emitting one would put `""` in the generated
     `string.concat`. *)
  let emit_chunk text pos next =
    if text = "" then next () else (Parser.INTERPOLATED_STRING text, pos)
  in
  let rec token () =
    match Queue.take_opt pending with
      | Some t -> t
      | None when !in_string -> (
          let { sep; _ } = List.hd !open_strings in
          let startp, _ = positions () in
          match read_chunk sep with
            | `Interpolation text ->
                in_string := false;
                emit_chunk text (startp, snd (positions ())) token
            | `Done text ->
                in_string := false;
                open_strings := List.tl !open_strings;
                let pos = (startp, snd (positions ())) in
                Queue.add (Parser.END_INTERPOLATION, pos) pending;
                emit_chunk text pos token)
      | None -> (
          match Lexer.token lexbuf with
            | Parser.PP_STRING_START sep -> (
                let startp, _ = positions () in
                match read_chunk sep with
                  | `Done text ->
                      (Parser.STRING (sep, text), (startp, snd (positions ())))
                  | `Interpolation text ->
                      let pos = (startp, snd (positions ())) in
                      open_strings := { sep; depth = 0 } :: !open_strings;
                      if text <> "" then
                        Queue.add (Parser.INTERPOLATED_STRING text, pos) pending;
                      (Parser.BEGIN_INTERPOLATION sep, pos))
            | Parser.PP_REGEXP (r, flags, pos) -> (Parser.REGEXP (r, flags), pos)
            (* `3.{a = 1}` lexes as one token so that `3.` is not taken for a
               float followed by a record. *)
            | Parser.PP_INT_DOT_LCUR n ->
                let spos, epos = positions () in
                let back n pos =
                  { pos with Lexing.pos_cnum = pos.Lexing.pos_cnum - n }
                in
                Queue.add (Parser.DOT, (back 2 spos, back 1 epos)) pending;
                Queue.add (Parser.LCUR, (back 1 spos, epos)) pending;
                (Parser.INT n, (spos, back 2 epos))
            | Parser.DOTVAR v ->
                let pos = positions () in
                Queue.add (Parser.VAR v, pos) pending;
                (Parser.DOT, pos)
            | Parser.NULLDOT ->
                let pos = positions () in
                Queue.add (Parser.DOT, pos) pending;
                (Parser.VAR "_null", pos)
            | Parser.LCUR when !open_strings <> [] ->
                let frame = List.hd !open_strings in
                frame.depth <- frame.depth + 1;
                (Parser.LCUR, positions ())
            | Parser.RCUR when !open_strings <> [] ->
                let frame = List.hd !open_strings in
                if frame.depth = 0 then (
                  (* Closes the interpolation: back to reading the string. *)
                  in_string := true;
                  token ())
                else (
                  frame.depth <- frame.depth - 1;
                  (Parser.RCUR, positions ()))
            | token -> (token, positions ()))
  in
  token

(** Change MINUS to UMINUS if the minus is not preceded by a number (or an
    expression which could produce a number). *)
let uminus tokenizer =
  let no_uminus = ref false in
  let token () =
    match tokenizer () with
      | ( Parser.INT _, _
        | Parser.FLOAT _, _
        | Parser.VAR _, _
        | Parser.RPAR, _
        | Parser.RBRA, _
        | Parser.RCUR, _ ) as t ->
          no_uminus := true;
          t
      | Parser.MINUS, pos when not !no_uminus ->
          no_uminus := false;
          (Parser.UMINUS, pos)
      | t ->
          no_uminus := false;
          t
  in
  token

(* Last but not least: remove new lines and merge some tokens around them
   in order to remove some ambiguities, typically between:
   def foo \n (x,y) ... << Normal definition, starting with a couple
   def foo(x,y) ...     << Definition of the function foo *)
let strip_newlines tokenizer =
  let state = ref None in
  let rec token () =
    let inject_varlpar var v =
      match tokenizer () with
        | Parser.LPAR, (_, endp) ->
            state := None;
            let startp = fst (snd v) in
            (Parser.VARLPAR var, (startp, endp))
        | Parser.LBRA, (_, endp) when var <> "in" ->
            state := None;
            let startp = fst (snd v) in
            (Parser.VARLBRA var, (startp, endp))
        | Parser.PP_ENDL, _ ->
            state := None;
            v
        | x ->
            state := Some x;
            v
    in
    match !state with
      | None -> (
          match tokenizer () with
            | Parser.PP_ENDL, _ -> token ()
            | ((Parser.NULL, _) as v)
            | ((Parser.UNDERSCORE, _) as v)
            | ((Parser.VAR _, _) as v) ->
                state := Some v;
                token ()
            | x -> x)
      | Some ((Parser.VAR var, _) as v) -> inject_varlpar var v
      | Some ((Parser.UNDERSCORE, _) as v) -> inject_varlpar "_" v
      | Some ((Parser.NULL, _) as v) -> inject_varlpar "_null" v
      | Some x ->
          state := None;
          x
  in
  token

(* Wrap the lexer with its extensions *)
let mk_tokenizer ?fname lexbuf =
  let tokenizer = mk_tokenizer ?fname lexbuf |> uminus |> strip_newlines in
  fun () ->
    let t, (startp, endp) = tokenizer () in
    (t, startp, endp)
