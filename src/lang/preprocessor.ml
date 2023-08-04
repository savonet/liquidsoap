(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type tokenizer = unit -> Parser.token * Pos.t

let mk_tokenizer ?(fname = "") lexbuf =
  Sedlexing.set_filename lexbuf fname;
  fun () ->
    match Lexer.token lexbuf with
      | Parser.PP_STRING (s, pos) -> (Parser.STRING s, pos)
      | Parser.PP_REGEXP (r, flags, pos) -> (Parser.REGEXP (r, flags), pos)
      | token -> (token, Sedlexing.lexing_bytes_positions lexbuf)

(* The Lang_lexer is not quite enough for our needs, so we first define
   convenient layers between it and the parser. First a pre-processor which
   evaluates %ifdefs. *)
let eval_ifdefs tokenizer =
  let state = ref 0 in
  let rec token () =
    let go_on () =
      incr state;
      token ()
    in
    let rec skip () =
      match tokenizer () with
        | Parser.PP_ENDIF, _ -> token ()
        | Parser.PP_ELSE, _ -> go_on ()
        | _ -> skip ()
    in
    match tokenizer () with
      | (Parser.PP_IFDEF v, _ | Parser.PP_IFNDEF v, _) as tok ->
          let test =
            match fst tok with
              | Parser.PP_IFDEF _ -> fun x -> x
              | Parser.PP_IFNDEF _ -> not
              | _ -> assert false
          in
          (* XXX Less natural meaning than the original one. *)
          if test (Environment.has_builtin v) then go_on () else skip ()
      | Parser.PP_IFVERSION (cmp, ver), _ ->
          let current = Lang_string.Version.of_string Build_config.version in
          let ver = Lang_string.Version.of_string ver in
          let test =
            let compare = Lang_string.Version.compare current ver in
            match cmp with
              | `Eq -> compare = 0
              | `Geq -> compare >= 0
              | `Leq -> compare <= 0
              | `Gt -> compare > 0
              | `Lt -> compare < 0
          in
          if test then go_on () else skip ()
      | (Parser.PP_IFENCODER, _ | Parser.PP_IFNENCODER, _) as tok ->
          let has_enc =
            try
              let fmt =
                let token = fst (tokenizer ()) in
                match token with
                  | Parser.ENCODER e ->
                      !Hooks.make_encoder ~pos:None (Term.make Term.unit) (e, [])
                  | _ -> failwith "expected an encoding format after %ifencoder"
              in
              !Hooks.has_encoder fmt
            with _ -> false
          in
          let test =
            if fst tok = Parser.PP_IFENCODER then fun x -> x else not
          in
          if test has_enc then go_on () else skip ()
      | Parser.PP_ELSE, _ ->
          if !state = 0 then failwith "no %ifdef to end here";
          decr state;
          skip ()
      | Parser.PP_ENDIF, _ ->
          if !state = 0 then failwith "no %ifdef to end here";
          decr state;
          token ()
      | x -> x
  in
  token

(* The expander turns "bla #{e} bli" into ("bla "^string(e)^" bli"). *)
type exp_item =
  | String of string
  | Expr of tokenizer
  | Concat
  | RPar
  | LPar
  | String_of

let expand_string ?fname tokenizer =
  let state = Queue.create () in
  let add pos x = Queue.add (x, pos) state in
  let pop () = ignore (Queue.take state) in
  let parse s pos =
    let l = Regexp.split (Regexp.regexp "#{(.*?)}") s in
    let l = if l = [] then [""] else l in
    let add = add pos in
    let rec parse = function
      | s :: x :: l ->
          let lexbuf = Sedlexing.Utf8.from_string x in
          let tokenizer = mk_tokenizer ?fname lexbuf in
          let tokenizer () = (fst (tokenizer ()), pos) in
          List.iter add
            [String s; Concat; LPar; String_of; Expr tokenizer; RPar; RPar];
          if l <> [] then (
            add Concat;
            parse l)
      | [x] -> add (String x)
      | [] -> assert false
    in
    parse l
  in
  let rec token () =
    if Queue.is_empty state then (
      match tokenizer () with
        | Parser.STRING s, pos ->
            parse s pos;
            if Queue.length state > 1 then (
              add pos RPar;
              (Parser.LPAR, pos))
            else token ()
        | x -> x)
    else (
      let el, pos = Queue.peek state in
      match el with
        | String s ->
            pop ();
            (Parser.STRING s, pos)
        | Concat ->
            pop ();
            (Parser.BIN2 "^", pos)
        | RPar ->
            pop ();
            (Parser.RPAR, pos)
        | LPar ->
            pop ();
            (Parser.LPAR, pos)
        | String_of ->
            pop ();
            (Parser.VARLPAR "string", pos)
        | Expr tokenizer -> (
            match tokenizer () with
              | Parser.EOF, _ ->
                  pop ();
                  token ()
              | x, _ -> (x, pos)))
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

(* Replace DOTVAR v with DOT, VAR v *)
let dotvar tokenizer =
  let state = ref None in
  let token () =
    match !state with
      | Some t ->
          state := None;
          t
      | None -> (
          match tokenizer () with
            | Parser.DOTVAR v, pos ->
                state := Some (Parser.VAR v, pos);
                (Parser.DOT, pos)
            | t -> t)
  in
  token

(** Change MINUS to UMINUS if the minus is not preceded by a number (or an
   expression which could produce a number). *)
let uminus tokenizer =
  let was_number = ref false in
  let token () =
    match tokenizer () with
      | (Parser.INT _, _ | Parser.FLOAT _, _ | Parser.VAR _, _ | Parser.RPAR, _)
        as t ->
          was_number := true;
          t
      | Parser.MINUS, pos when not !was_number ->
          was_number := false;
          (Parser.UMINUS, pos)
      | t ->
          was_number := false;
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
            | (Parser.VAR _, _) as v ->
                state := Some v;
                token ()
            | x -> x)
      | Some ((Parser.VAR var, _) as v) -> inject_varlpar var v
      | Some ((Parser.UNDERSCORE, _) as v) -> inject_varlpar "_" v
      | Some x ->
          state := None;
          x
  in
  token

(* Wrap the lexer with its extensions *)
let mk_tokenizer ?fname lexbuf =
  let tokenizer =
    mk_tokenizer ?fname lexbuf |> eval_ifdefs |> expand_string ?fname
    |> int_meth |> dotvar |> uminus |> strip_newlines
  in
  fun () ->
    let t, (startp, endp) = tokenizer () in
    (t, startp, endp)
