(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

type t = bool array array
type bitmap = t

let create c width height : t = Array.init height (fun _ -> Array.make width c)
let create_white = create true
let create = create false
let make data : t = data

let init width height f =
  make (Array.init height (fun j -> Array.init width (fun i -> f i j)))

let width (img : t) = if Array.length img = 0 then 0 else Array.length img.(0)
let height (img : t) = Array.length img
let get_pixel img i j = img.(j).(i)
let set_pixel img i j c = img.(j).(i) <- c

let fill img f =
  for j = 0 to height img - 1 do
    for i = 0 to width img - 1 do
      set_pixel img i j (f i j)
    done
  done

let scale src tgt =
  let ws = width src in
  let wt = width tgt in
  let hs = height src in
  let ht = height tgt in
  fill tgt (fun i j -> get_pixel src (i * ws / wt) (j * hs / ht))

let rescale p q img =
  let img2 = create (width img * p / q) (height img * p / q) in
  scale img img2;
  img2

let blit src ?(x = 0) ?(y = 0) dst =
  let width = min (width src) (width dst - x) in
  let height = min (height src) (height dst - y) in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      set_pixel dst (x + i) (y + j) (get_pixel src i j)
    done
  done

(** Bitmap fonts. *)
module Font = struct
  module CharMap = Map.Make (struct
    type t = char

    let compare (c : t) (d : t) = Stdlib.compare c d
  end)

  (** A fixed-size font. *)
  type nonrec t = {
    map : t CharMap.t Lazy.t;
    width : int;  (** width of a char in pixels *)
    height : int;  (** height of a char in pixels *)
    default : t;  (** default displayed character when not supported *)
    uppercase : bool;  (** whether only uppercase characters are supported *)
    char_space : int;
    line_space : int;
  }

  let height font = font.height

  (** Our native font. *)
  let native : t =
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
        ('<', [| "  *"; " * "; "*  "; " * "; "  *" |]);
        ('>', [| "*  "; " * "; "  *"; " * "; "*  " |]);
      ]
    in
    let width = 3 in
    let height = 5 in
    let map =
      Lazy.from_fun (fun () ->
          List.fold_left
            (fun f (c, b) ->
              let bmp = init width height (fun i j -> b.(j).[i] <> ' ') in
              CharMap.add c bmp f)
            CharMap.empty prebitmap)
    in
    let default = create_white width height in
    {
      map;
      width;
      height;
      default;
      uppercase = true;
      char_space = 1;
      line_space = 2;
    }

  let render ?(font = native) ?size text =
    let height = Option.value ~default:font.height size in
    let text_height, text_width =
      let h = ref 1 in
      let max = ref 0 in
      let cur = ref 0 in
      for i = 0 to String.length text - 1 do
        if text.[i] = '\n' then (
          max := Stdlib.max !max !cur;
          cur := 0;
          incr h)
        else incr cur
      done;
      max := Stdlib.max !max !cur;
      (!h, !max)
    in
    let img =
      let width =
        (text_width * font.width) + ((text_width - 1) * font.char_space)
      in
      let height =
        (text_height * font.height) + ((text_height - 1) * font.line_space)
      in
      let width = max width 0 in
      let height = max height 0 in
      create width height
    in
    let xoff = ref 0 in
    let yoff = ref 0 in
    for i = 0 to String.length text - 1 do
      let c = text.[i] in
      if c = '\n' then (
        xoff := 0;
        yoff := !yoff + font.height + font.line_space)
      else (
        let c = if font.uppercase then Char.uppercase_ascii c else c in
        let c =
          match CharMap.find_opt c (Lazy.force font.map) with
            | Some c -> c
            | None -> font.default
        in
        blit c ~x:!xoff ~y:!yoff img;
        xoff := !xoff + font.width + font.char_space)
    done;
    rescale height font.height img
end
