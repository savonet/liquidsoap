(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

module Term = Lang_values
module T = Lang_types

(** Runtime error, should eventually disappear. *)
exception Invalid_value of Term.V.value*string

exception Clock_conflict of (T.pos option * string * string)
exception Clock_loop of (T.pos option * string * string)

let error    = Console.colorize [`red;`bold] "Error"
let warning  = Console.colorize [`yellow;`bold] "Warning"

let position pos =
  Console.colorize [`white;`bold] (String.capitalize_ascii pos)

let error_header pos =
  Format.printf "@[<7>%s:\n%s: " (position pos) error

let warning_header pos =
  Format.printf "@[<9>%s:\n%s: " (position pos) warning

(** Exception raised by report_error after an error has been displayed.
  * Unknown errors are re-raised, so that their content is not totally lost. *)
exception Error

let strict = ref false

let report lexbuf f =
  let print_error error =
    flush_all () ;
    let pos =
      let start = lexbuf.Sedlexing_compat.lex_curr_p in
      let buf = Sedlexing_compat.Utf8.lexeme lexbuf in
      Printf.sprintf "%sine %d, char %d%s"
        (if start.Lexing.pos_fname="" then "L" else
           Printf.sprintf "File %S, l" start.Lexing.pos_fname)
        start.Lexing.pos_lnum
        (1+start.Lexing.pos_cnum-start.Lexing.pos_bol)
        (if buf = "" then "" else (Printf.sprintf " before %S" buf)) ;
    in
    error_header pos;
    Format.printf "%s\n@]@." error
  in
    try f () with
      | Failure s when s = "lexing: empty token" -> print_error "Empty token" ; raise Error
      | Parsing.Parse_error -> print_error "Parse error" ; raise Error
      | Lang_values.Parse_error (pos,s) ->
        let pos = T.print_pos pos in
        error_header pos;
        Format.printf "@%s@]@." s;
        raise Error
      | Term.Unbound (pos,s) ->
          let pos = T.print_pos (Utils.get_some pos) in
          error_header pos;
          Format.printf "Undefined variable %s@]@." s;
          raise Error
      | T.Type_Error explain ->
          flush_all () ;
          Lang_types.print_type_error error_header explain ;
          raise Error
      | Term.No_label (f,lbl,first,x) ->
          let pos_f =
            T.print_pos (Utils.get_some f.Term.t.T.pos)
          in
          let pos_x = T.print_pos (Utils.get_some x.Term.t.T.pos) in
          flush_all () ;
          error_header pos_x ;  
          Format.printf "Cannot apply that parameter because the function %s@ has %s@ %s!@]@."
              pos_f
              (if first then "no" else "no more")
              (if lbl="" then "unlabeled argument" else
                 Format.sprintf "argument labeled %S" lbl) ;
          raise Error
      | Term.Ignored tm ->
          flush_all () ;
          warning_header (T.print_pos (Utils.get_some tm.Term.t.T.pos));
          Format.printf "Ignored value.@]@.";
          if !strict then raise Error
      | Term.Unused_variable (s,pos) ->
          flush_all () ;
          warning_header (T.print_single_pos pos);
          Format.printf "Unused variable %s@]@." s;
          if !strict then raise Error
      | Invalid_value (v,msg) ->
          error_header (T.print_pos (Utils.get_some v.Term.V.t.T.pos));
          Format.printf "Invalid value:@ %s@.]@." msg;
          raise Error
      | Lang_encoders.Error (v,s) ->
          error_header (T.print_pos (Utils.get_some v.Lang_values.t.T.pos));
          Format.printf "Error in encoding format: %s@.]@." s;
          raise Error
      | Failure s ->
          print_error (Printf.sprintf "failure: %s" s);
          raise Error
      | Clock_conflict (pos,a,b) ->
          (* TODO better printing of clock errors: we don't have position
           *   information, use the source's ID *)
          error_header (T.print_pos (Utils.get_some pos));
          Format.printf "A source cannot belong to two clocks (%s,@ %s).@]@."
            a b ;
          raise Error
      | Clock_loop (pos,a,b) ->
          error_header (T.print_pos (Utils.get_some pos));
          Format.printf "Cannot unify two nested clocks (%s,@ %s).@]@."
            a b ;
          raise Error
      | End_of_file -> raise End_of_file
      | e -> print_error "Unknown error" ; raise e
