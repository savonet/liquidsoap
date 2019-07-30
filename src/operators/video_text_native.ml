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

let prebitmap =
  [|
    [|
      " * ";
      "* *";
      "***";
      "* *";
      "* *";
    |];
    [|
      "** ";
      "* *";
      "** ";
      "* *";
      "** ";
    |];
    [|
      " **";
      "*  ";
      "*  ";
      "*  ";
      " **";
    |];
    [|
      "** ";
      "* *";
      "* *";
      "* *";
      "** ";
    |];
    [|
      "***";
      "*  ";
      "** ";
      "*  ";
      "***";
    |];
    [|
      "***";
      "*  ";
      "** ";
      "*  ";
      "*  ";
    |];
    [|
      " **";
      "*  ";
      "* *";
      "* *";
      " **";
    |];
    [|
      "* *";
      "* *";
      "***";
      "* *";
      "* *";
    |];
    [|
      " * ";
      "   ";
      " * ";
      " * ";
      " * ";
    |];
    [|
      "  *";
      "  *";
      "  *";
      "* *";
      " * ";
    |];
    [|
      "* *";
      "** ";
      "*  ";
      "** ";
      "* *";
    |];
    [|
      "*  ";
      "*  ";
      "*  ";
      "*  ";
      "***";
    |];
    [|
      "* *";
      "***";
      "* *";
      "* *";
      "* *";
    |];
    [|
      "* *";
      "***";
      "***";
      "***";
      "* *";
    |];
    [|
      " * ";
      "* *";
      "* *";
      "* *";
      " * ";
    |];
    [|
      "** ";
      "* *";
      "** ";
      "*  ";
      "*  ";
    |];
    [|
      " * ";
      "* *";
      "* *";
      "* *";
      " **";
    |];
    [|
      "** ";
      "* *";
      "** ";
      "* *";
      "* *";
    |];
    [|
      " **";
      "*  ";
      " * ";
      "  *";
      "** ";
    |];
    [|
      "***";
      " * ";
      " * ";
      " * ";
      " * ";
    |];
    [|
      "* *";
      "* *";
      "* *";
      "* *";
      "***";
    |];
    [|
      "* *";
      "* *";
      "* *";
      "* *";
      " * ";
    |];
    [|
      "* *";
      "* *";
      "* *";
      "***";
      "* *";
    |];
    [|
      "* *";
      "* *";
      " * ";
      "* *";
      "* *";
    |];
    [|
      "* *";
      "* *";
      " * ";
      " * ";
      " * ";
    |];
    [|
      "***";
      "  *";
      " * ";
      "*  ";
      "***";
    |];
    [|
      " * ";
      "* *";
      "* *";
      "* *";
      " * ";
    |];
    [|
      " * ";
      "** ";
      " * ";
      " * ";
      " * ";
    |];
    [|
      " * ";
      "* *";
      "  *";
      " * ";
      "***";
    |];
    [|
      "** ";
      "  *";
      " * ";
      "  *";
      "** ";
    |];
    [|
      "  *";
      " **";
      "***";
      "  *";
      "  *";
    |];
    [|
      "***";
      "*  ";
      "** ";
      "  *";
      "** ";
    |];
    [|
      " **";
      "*  ";
      "** ";
      "* *";
      " * ";
    |];
    [|
      "***";
      "  *";
      " * ";
      " * ";
      " * ";
    |];
    [|
      " * ";
      "* *";
      " * ";
      "* *";
      " * ";
    |];
    [|
      " * ";
      "* *";
      " **";
      "  *";
      " * ";
    |];
    [|
      "   ";
      "   ";
      "   ";
      "   ";
      " * ";
    |];
    [|
      "   ";
      "   ";
      "   ";
      " * ";
      " * ";
    |];
    [|
      " * ";
      " * ";
      " * ";
      "   ";
      " * ";
    |];
    [|
      " * ";
      "* *";
      " **";
      " * ";
      " * ";
    |];
  |]

let bitmap = ref [||]

let init () =
  bitmap :=
    Array.map
      (fun a ->
        Array.map
          (fun s ->
            Array.init (String.length s) (fun i -> s.[i] <> ' ')
          ) a
      ) prebitmap

let char_height = 5

let char_width = 3

let char c =
  let bitmap = !bitmap in
  let c = Char.uppercase_ascii c in
  if 'A' <= c && c <= 'Z' then bitmap.(int_of_char c - int_of_char 'A')
  else if '0' <= c && c <= '9' then bitmap.(int_of_char c - int_of_char '0' + 26)
  else if c = '.' then bitmap.(36)
  else if c = ',' then bitmap.(37)
  else if c = '!' then bitmap.(38)
  else if c = '?' then bitmap.(39)
  else if c = ' ' then Array.make char_height (Array.make char_width false)
  else Array.make char_height (Array.make char_width true)

(** Space between chars. *)
let char_space = 1

let render_text ~font ~size text =
  (* TODO: we ignore font for now... *)
  let () = ignore font in
  let h = char_height in
  let bmp =
    let ans = Array.make h [||] in
    for i = 0 to String.length text - 1 do
      if i <> 0 then
        for j = 0 to h - 1 do
          ans.(j) <- Array.append ans.(j) (Array.make char_space false)
        done;
      let c = char text.[i] in
      for j = 0 to h - 1 do
        ans.(j) <- Array.append ans.(j) c.(j)
      done
    done;
    ans
  in
  let w = Array.length bmp.(0) in
  let get_pixel x y =
    let x = x * char_height / size in
    let y = y * char_height / size in
    if 0 <= y && y <  h && 0 <= x && x < w then
      if bmp.(y).(x) then 0xff else 0x00
    else 0x00
  in
  let h = h * size / char_height in
  let w = w * size / char_height in
  w, h, get_pixel

let () =
  Video_text.register "native" init render_text
