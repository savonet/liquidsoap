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

type tokenizer = unit -> Parser.token * Term_base.parsed_pos

let mk_tokenizer ?(fname = "") lexbuf =
  Sedlexing.set_filename lexbuf fname;
  fun () ->
    match Lexer.token lexbuf with
      | Parser.PP_STRING (c, s, pos) -> (Parser.STRING (c, s), pos)
      | Parser.PP_REGEXP (r, flags, pos) -> (Parser.REGEXP (r, flags), pos)
      | token -> (token, Sedlexing.lexing_bytes_positions lexbuf)

(* The expander turns "bla #{e} bli" into ("bla "^string(e)^" bli"). *)
type exp_item = String of string | Expr of tokenizer | End

exception Found_interpolation

let expand_string ?fname tokenizer =
  let state = Queue.create () in
  let add pos x = Queue.add (x, pos) state in
  let pop () = ignore (Queue.take state) in
  let clear () = Queue.clear state in
  let is_interpolating () =
    try
      Queue.iter
        (function Expr _, _ -> raise Found_interpolation | _ -> ())
        state;
      false
    with Found_interpolation -> true
  in
  let parse ~sep s pos =
    let rex = Re.Pcre.regexp "#\\{([^}]*)\\}" in
    let l = Re.Pcre.full_split ~rex s in
    let l = if l = [] then [Re.Pcre.Text s] else l in
    let add = add pos in
    let rec parse = function
      | Re.Pcre.Group (_, x) :: l ->
          let x = Lexer.render_string ~pos ~sep x in
          let lexbuf = Sedlexing.Utf8.from_string x in
          let tokenizer = mk_tokenizer ?fname lexbuf in
          let tokenizer () = (fst (tokenizer ()), pos) in
          add (Expr tokenizer);
          parse l
      | Re.Pcre.Text x :: l ->
          add (String x);
          parse l
      | Re.Pcre.NoGroup :: l | Re.Pcre.Delim _ :: l -> parse l
      | [] -> add End
    in
    parse l
  in
  let rec token () =
    if Queue.is_empty state then (
      match tokenizer () with
        | (Parser.STRING (sep, s), pos) as tok ->
            parse ~sep s pos;
            if is_interpolating () then (Parser.BEGIN_INTERPOLATION sep, pos)
            else (
              clear ();
              tok)
        | x -> x)
    else (
      let el, pos = Queue.peek state in
      match el with
        | String s ->
            pop ();
            (Parser.INTERPOLATED_STRING s, pos)
        | Expr tokenizer -> (
            match tokenizer () with
              | Parser.EOF, _ ->
                  pop ();
                  token ()
              | x, _ -> (x, pos))
        | End ->
            pop ();
            (Parser.END_INTERPOLATION, pos))
  in
  token

(** Special token in order to avoid 3.{s = "a"} to be parsed as a float followed
    by a record. *)
let int_meth tokenizer =
  let q = Queue.create () in
  let fill () =
    match tokenizer () with
      | Parser.PP_INT_DOT_LCUR n, (spos, epos) ->
          let a n pos =
            { pos with Lexing.pos_cnum = pos.Lexing.pos_cnum - n }
          in
          Queue.add_seq q
            (List.to_seq
               [
                 (Parser.INT n, (spos, a 2 epos));
                 (Parser.DOT, (a 2 spos, a 1 epos));
                 (Parser.LCUR, (a 1 spos, epos));
               ])
      | t -> Queue.add t q
  in
  let token () =
    if Queue.is_empty q then fill ();
    Queue.pop q
  in
  token

(* Replace DOTVAR v with DOT, VAR v
   and NULLDOT with "_null", DOT *)
let dotter tokenizer =
  let state = ref None in
  let token () =
    match !state with
      | Some t ->
          state := None;
          t
      | None -> (
          match tokenizer () with
            | Parser.NULLDOT, pos ->
                state := Some (Parser.DOT, pos);
                (Parser.VAR "_null", pos)
            | Parser.DOTVAR v, pos ->
                state := Some (Parser.VAR v, pos);
                (Parser.DOT, pos)
            | t -> t)
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
  let tokenizer =
    mk_tokenizer ?fname lexbuf |> expand_string ?fname |> int_meth |> dotter
    |> uminus |> strip_newlines
  in
  fun () ->
    let t, (startp, endp) = tokenizer () in
    (t, startp, endp)
