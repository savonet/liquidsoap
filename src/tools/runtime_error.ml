(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** An error at runtime. *)

(** Positions *)

type pos = Lexing.position * Lexing.position

let print_pos ?(prefix = "at ") (start, stop) =
  let prefix =
    match start.Lexing.pos_fname with
      | "" -> prefix
      | file -> prefix ^ file ^ ", "
  in
  let f l = (l.Lexing.pos_lnum, l.Lexing.pos_cnum - l.Lexing.pos_bol) in
  let lstart, cstart = f start in
  let lstop, cstop = f stop in
  if lstart = lstop then
    if cstop = cstart + 1 then
      Printf.sprintf "%sline %d, char %d" prefix lstart cstart
    else Printf.sprintf "%sline %d, char %d-%d" prefix lstart cstart cstop
  else
    Printf.sprintf "%sline %d char %d - line %d char %d" prefix lstart cstart
      lstop cstop

let print_pos_opt ?prefix = function
  | Some pos -> print_pos ?prefix pos
  | None -> "unknown position"

let rec print_pos_list ?prefix = function
  | [] -> "unknown position"
  | [pos] -> print_pos ?prefix pos
  | pos :: l -> print_pos_list ?prefix l ^ ", " ^ print_pos ?prefix pos

(** Given a position, find the relevant excerpt. *)
let excerpt (start, stop) =
  try
    if start.Lexing.pos_fname <> stop.Lexing.pos_fname then raise Exit;
    let fname = start.Lexing.pos_fname in
    let l1 = start.Lexing.pos_lnum in
    let l2 = stop.Lexing.pos_lnum in
    let ic = open_in fname in
    let n = ref 1 in
    while !n < l1 do
      ignore (input_line ic);
      incr n
    done;
    let lines = ref [] in
    while !n <= l2 do
      lines := input_line ic :: !lines;
      incr n
    done;
    close_in ic;
    let lines = List.rev !lines in
    let s = String.concat "\n" lines ^ "\n" in
    Some s
  with _ -> None

let excerpt_opt = function Some pos -> excerpt pos | None -> None

type runtime_error = { kind : string; msg : string; pos : pos list }

exception Runtime_error of runtime_error

let () =
  Printexc.register_printer (function
    | Runtime_error { kind; msg; pos } ->
        Some
          (Printf.sprintf "Lang.Runtime_error { kind: %s, msg: %s, pos: [%s] }"
             (Utils.quote_string kind) (Utils.quote_string msg)
             (String.concat ", " (List.map (fun pos -> print_pos pos) pos)))
    | _ -> None)

let error ?bt ?(pos = []) ?(message = "") kind =
  let e = Runtime_error { kind; msg = message; pos } in
  match bt with
    | None -> raise e
    | Some bt -> Printexc.raise_with_backtrace e bt
