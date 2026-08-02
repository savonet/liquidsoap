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

(** Operations on positions (in source files). *)

(** A position. *)
type t = bytes

type pos = {
  fname : string;
  lstart : int;
  lstop : int;
  cstart : int;
  cstop : int;
}

let pack_offset = 8

let pack { fname; lstart; lstop; cstart; cstop } =
  let b = Buffer.create (String.length fname + pack_offset) in
  Buffer.add_uint16_ne b lstart;
  Buffer.add_uint16_ne b lstop;
  Buffer.add_uint16_ne b cstart;
  Buffer.add_uint16_ne b cstop;
  Buffer.add_string b fname;
  Buffer.to_bytes b

let unpack s =
  let fname_len = Bytes.length s - pack_offset in
  let fname = Bytes.create fname_len in
  Bytes.blit s pack_offset fname 0 fname_len;
  let lstart = Bytes.get_uint16_ne s 0 in
  let lstop = Bytes.get_uint16_ne s 2 in
  let cstart = Bytes.get_uint16_ne s 4 in
  let cstop = Bytes.get_uint16_ne s 6 in
  { fname = Bytes.unsafe_to_string fname; lstart; lstop; cstart; cstop }

let of_lexing_pos (start, stop) =
  let f l = (l.Lexing.pos_lnum, l.Lexing.pos_cnum - l.Lexing.pos_bol) in
  let lstart, cstart = f start in
  let lstop, cstop = f stop in
  pack { fname = start.Lexing.pos_fname; lstart; cstart; lstop; cstop }

let to_string ?(prefix = "at ") pos =
  let { fname; lstart; lstop; cstart; cstop } = unpack pos in
  let prefix = match fname with "" -> prefix | file -> prefix ^ file ^ ", " in
  if lstart = lstop then
    if cstop = cstart + 1 then
      Printf.sprintf "%sline %d, char %d" prefix lstart cstart
    else Printf.sprintf "%sline %d, char %d-%d" prefix lstart cstart cstop
  else
    Printf.sprintf "%sline %d char %d - line %d char %d" prefix lstart cstart
      lstop cstop

let string_of_pos = to_string

module Option = struct
  type base = t
  type t = base option

  let to_string ?prefix : t -> string = function
    | Some pos -> to_string ?prefix pos
    | None -> "unknown position"
end

module List = struct
  type base = t
  type t = base list

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
