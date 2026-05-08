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

open ImageBase
module BGRA = ImageBGRA

module Color = struct
  type t = int * int * int * int
end

type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  (* Order matters for C callbacks! *)
  data : data;
  width : int;
  height : int;
  stride : int;
}

let width buf = buf.width
let height buf = buf.height
let dimensions buf = (buf.width, buf.height)
let data buf = buf.data
let size buf = Bigarray.Array1.dim buf.data
let stride buf = buf.stride

let make ?stride width height data =
  let stride = match stride with Some v -> v | None -> 4 * width in
  { data; width; height; stride }

let create ?stride width height =
  let stride = match stride with Some v -> v | None -> 4 * width in
  let stride, data = Data.rounded_plane stride height in
  make ~stride width height data

let copy f =
  let nf = create ~stride:f.stride f.width f.height in
  Bigarray.Array1.blit f.data nf.data;
  nf

(* Remove the optional stride argument. *)
let create width height = create width height

external blit : t -> t -> unit = "caml_rgb_blit"
external blit_off : t -> t -> int -> int -> bool -> unit = "caml_rgb_blit_off"

external blit_off_scale : t -> t -> int * int -> int * int -> bool -> unit
  = "caml_rgb_blit_off_scale"

let blit_all src dst =
  assert (
    src.width = dst.width && src.height = dst.height && src.stride = dst.stride);
  blit src dst

let blit ?(blank = true) ?(x = 0) ?(y = 0) ?w ?h src dst =
  match (w, h) with
    | None, None -> blit_off src dst x y blank
    | Some w, Some h -> blit_off_scale src dst (x, y) (w, h) blank
    | _, _ -> assert false

external fill_all : t -> Color.t -> unit = "caml_rgb_fill"
external blank_all : t -> unit = "caml_rgb_blank"

let blank = blank_all

external fill_alpha : t -> int -> unit = "caml_rgb_fill_alpha"
external of_RGB24_string : t -> string -> unit = "caml_rgb_of_rgb8_string"

let of_RGB24_string data width =
  let height = String.length data / 3 / width in
  let ans = create width height in
  of_RGB24_string ans data;
  ans

external of_BGRA : t -> BGRA.t -> unit = "caml_rgba_of_bgra"

let of_BGRA bgra =
  let img = create bgra.BGRA.width bgra.BGRA.height in
  of_BGRA img bgra;
  img

external to_BGRA : BGRA.t -> t -> unit = "caml_rgba_of_bgra"

let to_BGRA img =
  let bgra = BGRA.create img.width img.height in
  to_BGRA bgra img;
  bgra

external to_Gray8 : t -> Data.t -> unit = "caml_mm_RGBA8_to_Gray8"

let to_Gray8 rgb gray = to_Gray8 rgb gray.Gray8.data

let to_Gray8_create rgb =
  let gray = Gray8.create (width rgb) (height rgb) in
  to_Gray8 rgb gray;
  gray

external get_pixel : t -> int -> int -> Color.t = "caml_rgb_get_pixel"
external set_pixel : t -> int -> int -> Color.t -> unit = "caml_rgb_set_pixel"

let set_pixel img i j =
  assert (0 <= i && i < img.width);
  assert (0 <= j && j < img.height);
  set_pixel img i j

let get_pixel_rgba = get_pixel
let set_pixel_rgba = set_pixel

external randomize_all : t -> unit = "caml_rgb_randomize"

let randomize = randomize_all

module Scale = struct
  type kind = Linear | Bilinear

  external scale_coef : t -> t -> int * int -> int * int -> unit
    = "caml_rgb_scale"

  external bilinear_scale_coef : t -> t -> float -> float -> unit
    = "caml_rgb_bilinear_scale"

  let scale_coef_kind k src dst (dw, sw) (dh, sh) =
    match k with
      | Linear -> scale_coef src dst (dw, sw) (dh, sh)
      | Bilinear ->
          let x = float dw /. float sw in
          let y = float dh /. float sh in
          bilinear_scale_coef src dst x y

  let onto ?(kind = Linear) ?(proportional = false) src dst =
    let sw, sh = (src.width, src.height) in
    let dw, dh = (dst.width, dst.height) in
    if dw = sw && dh = sh then blit_all src dst
    else if not proportional then scale_coef_kind kind src dst (dw, sw) (dh, sh)
    else (
      let n, d = if dh * sw < sh * dw then (dh, sh) else (dw, sw) in
      scale_coef_kind kind src dst (n, d) (n, d))

  let create ?kind ?(copy = true) ?proportional src w h =
    if (not copy) && width src = w && height src = h then src
    else (
      let dst = create w h in
      onto ?kind ?proportional src dst;
      dst)
end

let scale ?proportional src dst = Scale.onto ?proportional src dst

external to_BMP : t -> string = "caml_rgb_to_bmp"
external to_RGB24_string : t -> string = "caml_image_to_rgb24"

exception Invalid_format of string

let of_PPM ?alpha data =
  let w, h, d, o =
    try
      (* TODO: make it usable without bound checks *)
      assert (data.[0] = 'P');
      assert (data.[1] = '6');
      assert (data.[2] = '\n');
      let n = ref 3 in
      let read_int () =
        let ans = ref 0 in
        let ( !! ) = int_of_char in
        while !!'0' <= !!(data.[!n]) && !!(data.[!n]) <= !!'9' do
          ans := (!ans * 10) + !!(data.[!n]) - !!'0';
          incr n
        done;
        assert (data.[!n] = ' ' || data.[!n] = '\n');
        incr n;
        !ans
      in
      if data.[!n] = '#' then (
        incr n;
        while data.[!n] <> '\n' do
          incr n
        done;
        incr n);
      let w = read_int () in
      let h = read_int () in
      let d = read_int () in
      (w, h, d, !n)
    with _ -> raise (Invalid_format "Not a PPM file.")
  in
  let datalen = String.length data - o in
  if d <> 255 then
    raise
      (Invalid_format
         (Printf.sprintf "Files of color depth %d are not handled." d));
  if datalen < 3 * w * h then
    raise
      (Invalid_format
         (Printf.sprintf "Got %d bytes of data instead of expected %d." datalen
            (3 * w * h)));
  let ans = create w h in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let r, g, b =
        ( int_of_char data.[o + (3 * ((j * w) + i)) + 0],
          int_of_char data.[o + (3 * ((j * w) + i)) + 1],
          int_of_char data.[o + (3 * ((j * w) + i)) + 2] )
      in
      let a =
        match alpha with
          | Some (ra, ga, ba) ->
              if r = ra && g = ga && b = ba then 0x00 else 0xff
          | None -> 0xff
      in
      set_pixel ans i j (r, g, b, a)
    done
  done;
  ans

external to_int_image : t -> int array array = "caml_rgb_to_color_array"

(*
  let to_int_image buf =
    let w = buf.width in
    let h = buf.height in
    Array.init
      h
      (fun j ->
        Array.init
          w
          (fun i ->
            let r,g,b,a = get_pixel buf i j in
            (r lsl 16) + (g lsl 8) + b
          )
      )
  *)

external add : t -> t -> unit = "caml_rgb_add"

let add_fast = add

external add_off : t -> t -> int -> int -> unit = "caml_rgb_add_off"

external add_off_scale : t -> t -> int * int -> int * int -> unit
  = "caml_rgb_add_off_scale"

let add ?(x = 0) ?(y = 0) ?w ?h src dst =
  match (w, h) with
    | None, None ->
        if x = 0 && y = 0 && src.width = dst.width && src.height = dst.height
        then add_fast src dst
        else add_off src dst x y
    | Some w, Some h -> add_off_scale src dst (x, y) (w, h)
    | _, _ -> assert false

external swap_rb : t -> unit = "caml_rgba_swap_rb"

module Effect = struct
  external greyscale : t -> bool -> unit = "caml_rgb_greyscale"

  let sepia buf = greyscale buf true
  let greyscale buf = greyscale buf false

  external invert : t -> unit = "caml_rgb_invert"
  external rotate : t -> float -> unit = "caml_rgb_rotate"

  external affine : t -> float -> float -> int -> int -> unit
    = "caml_rgb_affine"

  (* TODO: faster implementation? *)
  let translate f x y = affine f 1. 1. x y

  external flip : t -> unit = "caml_rgb_flip"
  external mask : t -> t -> unit = "caml_rgb_mask"
  external lomo : t -> unit = "caml_rgb_lomo"
  external box_blur : t -> unit = "caml_mm_RGBA8_box_blur"

  module Alpha = struct
    external scale : t -> float -> unit = "caml_rgb_scale_opacity"
    external blur : t -> unit = "caml_rgb_blur_alpha"
    external disk : t -> int -> int -> int -> unit = "caml_rgb_disk_opacity"

    external of_color_simple : t -> int * int * int -> int -> unit
      = "caml_rgb_color_to_alpha_simple"

    (* TODO: this does not work yet. *)
    (* external of_color : t -> int * int * int -> float -> float -> unit = "caml_rgb_color_to_alpha" *)
    let of_color = of_color_simple
  end
end

module Draw = struct
  external line : t -> int * int * int * int -> int * int -> int * int -> unit
    = "caml_mm_RGBA8_draw_line"
end

module Motion = struct
  (* TODO: compute old only once? *)
  let compute bs o n =
    Gray8.Motion.compute bs (to_Gray8_create o) (to_Gray8_create n)

  module Multi = struct
    include Motion_multi

    let compute bs o n =
      Gray8.Motion.Multi.compute bs (to_Gray8_create o) (to_Gray8_create n)

    external arrows : int -> vectors_data -> t -> unit
      = "caml_rgb_motion_multi_arrows"

    let arrows v img = arrows v.block_size v.vectors img
  end
end
