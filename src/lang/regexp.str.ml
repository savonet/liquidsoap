(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(* Abstract API for regexp *)

type t = Str.regexp

(* Str has some weird globals. 💩 *)
type sub = string * (string -> unit)

let regexp = Str.regexp
let regexp_or l = regexp (String.concat "\\|" l)
let split ~pat s = Str.split (regexp pat) s

let exec ?pat ?rex s =
  let rex =
    match (pat, rex) with
      | None, None -> failwith "At least one of pat or rex needs to be provided"
      | _, Some r -> r
      | Some r, None -> regexp r
  in
  if not (Str.string_match rex s 0) then raise Not_found;
  (s, fun s -> ignore (Str.string_match rex s 0))

let get_substring (s, fn) pos =
  fn s;
  Str.matched_group pos s

let substitute ?pat ?rex ~subst s =
  let rex =
    match (pat, rex) with
      | None, None -> failwith "At least one of pat or rex needs to be provided"
      | _, Some r -> r
      | Some r, None -> regexp r
  in
  Str.global_substitute rex subst s
