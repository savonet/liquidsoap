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

let prebitmap =
  [
    ('A', [| " * "; "* *"; "***"; "* *"; "* *" |]);
    ('B', [| "** "; "* *"; "** "; "* *"; "** " |]);
    ('C', [| " **"; "*  "; "*  "; "*  "; " **" |]);
    ('D', [| "** "; "* *"; "* *"; "* *"; "** " |]);
    ('E', [| "***"; "*  "; "** "; "*  "; "***" |]);
    ('F', [| "***"; "*  "; "** "; "*  "; "*  " |]);
    ('G', [| " **"; "*  "; "* *"; "* *"; " **" |]);
    ('H', [| "* *"; "* *"; "***"; "* *"; "* *" |]);
    ('I', [| " * "; " * "; " * "; " * "; " * " |]);
    ('J', [| "  *"; "  *"; "  *"; "* *"; " * " |]);
    ('K', [| "* *"; "** "; "*  "; "** "; "* *" |]);
    ('L', [| "*  "; "*  "; "*  "; "*  "; "***" |]);
    ('M', [| "* *"; "***"; "* *"; "* *"; "* *" |]);
    ('N', [| "* *"; "***"; "***"; "***"; "* *" |]);
    ('O', [| " * "; "* *"; "* *"; "* *"; " * " |]);
    ('P', [| "** "; "* *"; "** "; "*  "; "*  " |]);
    ('Q', [| " * "; "* *"; "* *"; "* *"; " **" |]);
    ('R', [| "** "; "* *"; "** "; "* *"; "* *" |]);
    ('S', [| " **"; "*  "; " * "; "  *"; "** " |]);
    ('T', [| "***"; " * "; " * "; " * "; " * " |]);
    ('U', [| "* *"; "* *"; "* *"; "* *"; "***" |]);
    ('V', [| "* *"; "* *"; "* *"; "* *"; " * " |]);
    ('W', [| "* *"; "* *"; "* *"; "***"; "* *" |]);
    ('X', [| "* *"; "* *"; " * "; "* *"; "* *" |]);
    ('Y', [| "* *"; "* *"; " * "; " * "; " * " |]);
    ('Z', [| "***"; "  *"; " * "; "*  "; "***" |]);
    ('0', [| " * "; "* *"; "* *"; "* *"; " * " |]);
    ('1', [| " * "; "** "; " * "; " * "; " * " |]);
    ('2', [| " * "; "* *"; "  *"; " * "; "***" |]);
    ('3', [| "** "; "  *"; " * "; "  *"; "** " |]);
    ('4', [| "  *"; " **"; "***"; "  *"; "  *" |]);
    ('5', [| "***"; "*  "; "** "; "  *"; "** " |]);
    ('6', [| " **"; "*  "; "** "; "* *"; " * " |]);
    ('7', [| "***"; "  *"; " * "; " * "; " * " |]);
    ('8', [| " * "; "* *"; " * "; "* *"; " * " |]);
    ('9', [| " * "; "* *"; " **"; "  *"; " * " |]);
    (' ', [| "   "; "   "; "   "; "   "; "   " |]);
    ('.', [| "   "; "   "; "   "; "   "; " * " |]);
    (',', [| "   "; "   "; "   "; " * "; " * " |]);
    ('!', [| " * "; " * "; " * "; "   "; " * " |]);
    ('?', [| " * "; "* *"; " **"; " * "; " * " |]);
    ('-', [| "   "; "   "; "***"; "   "; "   " |]);
    ('+', [| "   "; " * "; "***"; " * "; "   " |]);
    ('=', [| "   "; "***"; "   "; "***"; "   " |]);
    (':', [| "   "; " * "; "   "; " * "; "   " |]);
  ]

let bitmap = ref []

let init () =
  bitmap :=
    List.map
      (fun (c, a) ->
        ( c,
          Array.map
            (fun s -> Array.init (String.length s) (fun i -> s.[i] <> ' '))
            a ))
      prebitmap

let char_height = 5

let char c =
  let bitmap = !bitmap in
  let c = Char.uppercase_ascii c in
  try List.assoc c bitmap
  with Not_found -> Array.make char_height (Array.make 3 true)

(** Space between chars. *)
let char_space = 1

(** Space between lines. *)
let line_space = 2

let render_text ~font ~size text =
  (* TODO: we ignore font for now... *)
  let () = ignore font in
  (* Compute bitmap as a matrix of booleans. *)
  let bmp =
    let ans = ref (Array.make char_height [||]) in
    (* Current line. *)
    let line = ref 0 in
    for i = 0 to String.length text - 1 do
      (* Vertical offset. *)
      let voff = !line * (char_height + line_space) in
      if Array.length !ans.(voff) <> 0 then
        (* Add a space *)
        for j = 0 to char_height - 1 do
          !ans.(voff + j) <-
            Array.append !ans.(voff + j) (Array.make char_space false)
        done;
      if text.[i] = '\n' then (
        ans := Array.append !ans (Array.make (line_space + char_height) [||]);
        incr line )
      else (
        let c = char text.[i] in
        for j = 0 to char_height - 1 do
          !ans.(voff + j) <- Array.append !ans.(voff + j) c.(j)
        done )
    done;
    !ans
  in
  let h = Array.length bmp in
  let get_pixel x y =
    let x = x * char_height / size in
    let y = y * char_height / size in
    if 0 <= y && y < h && 0 <= x && x < Array.length bmp.(y) then
      if bmp.(y).(x) then 0xff else 0x00
    else 0x00
  in
  let w = Array.fold_left (fun w l -> max w (Array.length l)) 0 bmp in
  let h = h * size / char_height in
  let w = w * size / char_height in
  (w, h, get_pixel)

let () = Video_text.register "native" init render_text
