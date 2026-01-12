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

exception Invalid

let is_digit c = '0' <= c && c <= '9'

let check s =
  let i =
    if
      String.length s >= 3 && s.[0] = '\xef' && s.[1] = '\xbb' && s.[2] = '\xbf'
    then 3
    else 0
  in
  s.[i] = '1' && ((s.[i + 1] = '\r' && s.[i + 2] = '\n') || s.[i + 1] = '\n')

let check_file fname =
  try
    let ic = open_in_bin fname in
    let s = really_input_string ic 5 in
    close_in ic;
    check s
  with _ -> false

let seconds_of_time (h, m, s, ms) =
  (3600. *. float h) +. (60. *. float m) +. float s +. (float ms /. 1000.)

let parse s =
  (* UTF-8 BOM marker *)
  let s =
    if
      String.length s >= 3 && s.[0] = '\xef' && s.[1] = '\xbb' && s.[2] = '\xbf'
    then String.sub s 3 (String.length s - 3)
    else s
  in
  let s = String.split_on_char '\n' s |> List.map String.trim in
  let s = s |> List.rev |> List.tl |> List.rev in
  let s = Queue.of_seq (List.to_seq s) in
  let n = ref 1 in
  let ans = ref [] in
  while not (Queue.is_empty s) do
    let n' = Queue.take s in
    let n' =
      match int_of_string_opt n' with Some n' -> n' | None -> raise Invalid
    in
    if n' <> !n then raise Invalid;
    incr n;
    let t = if Queue.is_empty s then raise Invalid else Queue.take s in
    if String.length t <> 29 then raise Invalid;
    let t1, t2 =
      if t.[2] <> ':' || t.[5] <> ':' || t.[8] <> ',' then raise Invalid;
      if t.[19] <> ':' || t.[22] <> ':' || t.[25] <> ',' then raise Invalid;
      if String.sub t 12 5 <> " --> " then raise Invalid;
      try
        ( ( int_of_string (String.sub t 0 2),
            int_of_string (String.sub t 3 2),
            int_of_string (String.sub t 6 2),
            int_of_string (String.sub t 9 3) ),
          ( int_of_string (String.sub t 17 2),
            int_of_string (String.sub t 20 2),
            int_of_string (String.sub t 23 2),
            int_of_string (String.sub t 26 3) ) )
      with _ -> raise Invalid
    in
    let text =
      let ans = ref [] in
      while (not (Queue.is_empty s)) && Queue.peek s <> "" do
        ans := Queue.take s :: !ans
      done;
      assert (Queue.is_empty s || Queue.take s = "");
      List.rev !ans |> String.concat "\n"
    in
    ans := ((t1, t2), text) :: !ans
  done;
  List.rev !ans

let parse_file fname =
  let ic = open_in_bin fname in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  parse s
