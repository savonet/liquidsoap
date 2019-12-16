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
exception Invalid_value of Term.V.value * string

exception Clock_conflict of (T.pos option * string * string)
exception Clock_loop of (T.pos option * string * string)

let error = Console.colorize [`red; `bold] "Error"
let warning = Console.colorize [`magenta; `bold] "Warning"
let position pos = Console.colorize [`bold] (String.capitalize_ascii pos)

let error_header idx pos =
  Format.printf "@[%s:\n%s %i: " (position pos) error idx

let warning_header idx pos =
  Format.printf "@[%s:\n%s %i: " (position pos) warning idx

(** Exception raised by report_error after an error has been displayed.
  * Unknown errors are re-raised, so that their content is not totally lost. *)
exception Error

let strict = ref false

let report lexbuf f =
  let print_error idx error =
    flush_all ();
    let pos =
      let start = snd (Sedlexing.lexing_positions lexbuf) in
      let buf = Sedlexing.Utf8.lexeme lexbuf in
      Printf.sprintf "%sine %d, char %d%s"
        ( if start.Lexing.pos_fname = "" then "L"
        else Printf.sprintf "File %S, l" start.Lexing.pos_fname )
        start.Lexing.pos_lnum
        (start.Lexing.pos_cnum - start.Lexing.pos_bol)
        (if buf = "" then "" else Printf.sprintf " before %S" buf)
    in
    error_header idx pos;
    Format.printf "%s\n@]@." error
  in
  if Lang_values.conf_debug_errors#get then f ()
  else (
    try f () with
      (* Warnings *)
      | Term.Ignored tm when Term.is_fun (T.deref tm.Term.t) ->
          flush_all ();
          warning_header 1 (T.print_pos (Utils.get_some tm.Term.t.T.pos));
          Format.printf
            "This function application is partial,@ being of type %s.@ Maybe \
             some arguments are missing.@]@."
            (T.print tm.Term.t);
          if !strict then raise Error
      | Term.Ignored tm when Term.is_source (T.deref tm.Term.t) ->
          flush_all ();
          warning_header 2 (T.print_pos (Utils.get_some tm.Term.t.T.pos));
          Format.printf
            "This source is unused, maybe it needs to@ be connected to an \
             output.@]@.";
          if !strict then raise Error
      | Term.Ignored tm ->
          flush_all ();
          warning_header 3 (T.print_pos (Utils.get_some tm.Term.t.T.pos));
          Format.printf "This expression should have type unit.@]@.";
          if !strict then raise Error
      | Term.Unused_variable (s, pos) ->
          flush_all ();
          warning_header 4 (T.print_single_pos pos);
          Format.printf "Unused variable %s@]@." s;
          if !strict then raise Error
      (* Errors *)
      | Failure s when s = "lexing: empty token" ->
          print_error 1 "Empty token";
          raise Error
      | Parsing.Parse_error ->
          print_error 2 "Parse error";
          raise Error
      | Lang_values.Parse_error (pos, s) ->
          let pos = T.print_pos pos in
          error_header 3 pos;
          Format.printf "%s@]@." s;
          raise Error
      | Term.Unbound (pos, s) ->
          let pos = T.print_pos (Utils.get_some pos) in
          error_header 4 pos;
          Format.printf "Undefined variable %s@]@." s;
          raise Error
      | T.Type_Error explain ->
          flush_all ();
          Lang_types.print_type_error (error_header 5) explain;
          raise Error
      | Term.No_label (f, lbl, first, x) ->
          let pos_f = T.print_pos (Utils.get_some f.Term.t.T.pos) in
          let pos_x = T.print_pos (Utils.get_some x.Term.t.T.pos) in
          flush_all ();
          error_header 6 pos_x;
          Format.printf
            "Cannot apply that parameter because the function %s@ has %s@ \
             %s!@]@."
            pos_f
            (if first then "no" else "no more")
            ( if lbl = "" then "unlabeled argument"
            else Format.sprintf "argument labeled %S" lbl );
          raise Error
      | Invalid_value (v, msg) ->
          error_header 7 (T.print_pos (Utils.get_some v.Term.V.t.T.pos));
          Format.printf "Invalid value:@ %s@]@." msg;
          raise Error
      | Lang_encoders.Error (v, s) ->
          error_header 8 (T.print_pos (Utils.get_some v.Lang_values.t.T.pos));
          Format.printf "%s@]@." (String.capitalize_ascii s);
          raise Error
      | Failure s ->
          print_error 9 (Printf.sprintf "Failure: %s" s);
          raise Error
      | Clock_conflict (pos, a, b) ->
          (* TODO better printing of clock errors: we don't have position
           *   information, use the source's ID *)
          error_header 10 (T.print_pos (Utils.get_some pos));
          Format.printf "A source cannot belong to two clocks (%s,@ %s).@]@." a
            b;
          raise Error
      | Clock_loop (pos, a, b) ->
          error_header 11 (T.print_pos (Utils.get_some pos));
          Format.printf "Cannot unify two nested clocks (%s,@ %s).@]@." a b;
          raise Error
      | Lang_values.Unsupported_format (pos, fmt) ->
          let pos = T.print_pos pos in
          error_header 12 pos;
          Format.printf
            "Unsupported format: %s.@ You must be missing an optional \
             dependency.@]@."
            (Encoder.string_of_format fmt);
          raise Error
      | Sedlexing.MalFormed -> print_error 13 "Malformed file."
      | End_of_file -> raise End_of_file
      | e ->
          print_error (-1) "Unknown error";
          raise e )
