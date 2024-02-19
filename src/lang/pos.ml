(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

(** Operations on positions (in source files). *)

(** A position. *)
type t = Lexing.position * Lexing.position

let to_string ?(prefix = "at ") ((start, stop) : t) =
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

let string_of_pos = to_string

module Option = struct
  type nonrec t = t option

  let to_string ?prefix : t -> string = function
    | Some pos -> to_string ?prefix pos
    | None -> "unknown position"
end

module List = struct
  (** A list of positions, corresponding to a stack of calls. The toplevel one
      is the external caller and the last one is the callee. *)
  type nonrec t = t list

  (** The most relevant position in a call stack. *)
  let rec to_pos = function
    | [x] -> x
    | _ :: l -> to_pos l
    | [] -> raise Not_found

  let rec to_string ?(newlines = false) ?prefix = function
    | [] -> "unknown position"
    | [pos] -> string_of_pos ?prefix pos
    | pos :: l ->
        to_string ~newlines ?prefix l
        ^ (if newlines then ",\n" else ", ")
        ^ string_of_pos ?prefix pos
end
