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

exception Invalid_position
exception Invalid_dimensions

module List = struct
  include List

  let rec iter_right f = function
    | x :: l ->
        iter_right f l;
        f x
    | [] -> ()
end

module Data = struct
  type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (* Creates an 16-bytes aligned plane. Returns (stride*plane). *)
  (* external create_rounded_plane : int -> int -> int * t = "caml_data_aligned_plane" *)

  let alloc n =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.C_layout n

  (** [round n k] rounds [n] to the nearest upper multiple of [k]. *)
  let round k n = (n + (k - 1)) / k * k

  (** [aligned k n] allocates [n] bytes at a multiple of [k]. *)
  external aligned : int -> int -> t = "caml_data_aligned"

  (* Creates an 16-bytes aligned plane. Returns (stride*plane). *)
  let rounded_plane width height =
    let align = 16 in
    let stride = round 16 width in
    let data = aligned align (height * stride) in
    (stride, data)

  external to_string : t -> string = "caml_data_to_string"
  external to_bytes : t -> bytes = "caml_data_to_string"
  external of_string : string -> t = "caml_data_of_string"

  let blit_all src dst = Bigarray.Array1.blit src dst

  external blit : t -> int -> t -> int -> int -> unit = "caml_data_blit_off"

  (* [@@noalloc] *)

  external copy : t -> t = "caml_data_copy"

  let sub buf ofs len = Bigarray.Array1.sub buf ofs len
  let length img = Bigarray.Array1.dim img
  let size img = length img
  let get = Bigarray.Array1.get
  let fill buf x = Bigarray.Array1.fill buf x
end

module Pixel = struct
  type rgba = int * int * int * int
  type rgb = int * int * int
  type yuv = int * int * int
  type yuva = (int * int * int) * int

  module RGBA = struct
    type t = RGBA

    let black = (0, 0, 0, 0xff)
    let white = (0xff, 0xff, 0xff, 0xff)
    let transparent = (0, 0, 0, 0)
  end

  external yuv_of_rgb : rgb -> yuv = "caml_yuv_of_rgb"
  external rgb_of_yuv : yuv -> rgb = "caml_rgb_of_yuv"
end

module Point = struct
  type t = int * int

  let min (x, y) (x', y') = (min x x', min y y')
  let max (x, y) (x', y') = (max x x', max y y')
  let lt (x, y) (x', y') = x < x' && y < y'
  let le (x, y) (x', y') = x <= x' && y <= y'
  let neg (x, y) = (-x, -y)
end

module Fraction = struct
  type t = int * int

  let min (a, b) (a', b') = if a * b' < a' * b then (a, b) else (a', b')
end

module Draw = struct
  (* Besenham algorithm. *)
  let line p (sx, sy) (dx, dy) =
    let steep = abs (dy - sy) > abs (dx - sx) in
    let sx, sy, dx, dy = if steep then (sy, sx, dy, dx) else (sx, sy, dx, dy) in
    let sx, sy, dx, dy =
      if sx > dx then (dx, dy, sx, sy) else (sx, sy, dx, dy)
    in
    let deltax = dx - sx in
    let deltay = abs (dy - sy) in
    let error = ref (deltax / 2) in
    let ystep = if sy < dy then 1 else -1 in
    let j = ref sy in
    for i = sx to dx - 1 do
      if steep then p !j i else p i !j;
      error := !error - deltay;
      if !error < 0 then (
        j := !j + ystep;
        error := !error + deltax)
    done
end

module Motion_multi = struct
  type vectors_data =
    (int, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t

  type vectors = {
    vectors : vectors_data;
    vectors_width : int;
    block_size : int;
  }

  external median_denoise : int -> vectors_data -> unit
    = "caml_rgb_motion_multi_median_denoise"

  let median_denoise v = median_denoise v.vectors_width v.vectors

  external mean : int -> vectors_data -> int * int
    = "caml_rgb_motion_multi_mean"

  let mean v = mean v.vectors_width v.vectors
end

module RGB8 = struct
  module Color = struct
    type t = int * int * int

    let of_int n =
      if n land lnot 0xffffff <> 0 then raise (Invalid_argument "Not a color");
      ((n lsr 16) land 0xff, (n lsr 8) land 0xff, n land 0xff)

    let to_int (r, g, b) = (r lsl 16) + (g lsl 8) + b
  end
end

module ARGB8 = struct
  module Color = struct
    type t = int * RGB8.Color.t

    let of_int n : t = (n lsr 24, RGB8.Color.of_int (n land 0xffffff))
  end
end

module Gray8 = struct
  (* TODO: stride ? *)
  type t = { data : Data.t; width : int }

  let make w d = { data = d; width = w }

  (* Don't use create_rounded_plane here since there is not stride.. *)
  let create w h =
    make w
      (Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h))

  module Motion = struct
    external compute : int -> int -> Data.t -> Data.t -> int * int
      = "caml_mm_Gray8_motion_compute"

    let compute bs o n = compute bs n.width o.data n.data

    module Multi = struct
      include Motion_multi

      external compute : int -> int -> Data.t -> Data.t -> vectors_data
        = "caml_mm_Gray8_motion_multi_compute"

      let compute bs o n =
        {
          vectors = compute bs n.width o.data n.data;
          vectors_width = n.width / bs;
          block_size = bs;
        }
    end
  end
end
