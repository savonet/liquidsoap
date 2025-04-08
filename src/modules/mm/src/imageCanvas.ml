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

module type CanvasImage = sig
  type t

  val width : t -> int
  val height : t -> int
  val size : t -> int
  val create : int -> int -> t
  val blank : t -> unit
  val copy : t -> t
  val add : t -> ?x:int -> ?y:int -> t -> unit
  val has_alpha : t -> bool
  val fill_alpha : t -> int -> unit
  val set_pixel_rgba : t -> int -> int -> Pixel.rgba -> unit
  val randomize : t -> unit
  val scale : t -> t -> unit
end

(** A canvas of images. The structure is immutable but its elements might be
    returned and therefore should not be used in place. *)
module Canvas (I : CanvasImage) = struct
  module Element = struct
    type t = Image of (int * int) * I.t  (** An image at given offset. *)

    let size = function Image (_, img) -> I.size img

    let translate dx dy = function
      | Image ((x, y), img) -> Image ((x + dx, y + dy), img)
  end

  module E = Element

  type t = { width : int; height : int; elements : E.t list }

  let create width height = { width; height; elements = [] }
  let width c = c.width
  let height c = c.height
  let size c = List.fold_left (fun n e -> n + E.size e) 0 c.elements
  let planes c = List.length c.elements

  let make ?width ?height ?(x = 0) ?(y = 0) image =
    let width = Option.value ~default:(I.width image) width in
    let height = Option.value ~default:(I.height image) height in
    { width; height; elements = [E.Image ((x, y), image)] }

  let add c c' =
    (* assert ((c.width < 0 || c.width = c'.width) && (c.height < 0 || c.height = c'.height)); *)
    {
      width = c'.width;
      height = c'.height;
      elements = c.elements @ c'.elements;
    }

  (* TODO: improve precision with something like this:
     https://stackoverflow.com/questions/2628118/rectangles-covering *)
  let covering c =
    let width = width c in
    let height = height c in
    let covering_element = function
      | E.Image ((x, y), img) ->
          let w = I.width img in
          let h = I.height img in
          x <= 0 && y <= 0
          && x + w >= width
          && y + h >= height
          && not (I.has_alpha img)
    in
    List.exists covering_element c.elements

  let render ?(fresh = false) ?(transparent = true) c =
    assert (width c >= 0 && height c >= 0);
    match c.elements with
      | [Image ((0, 0), img)]
        when (not fresh) && I.width img = width c && I.height img = height c ->
          img
      | elements ->
          let r = I.create (width c) (height c) in
          if not (covering c) then (
            I.blank r;
            if transparent then I.fill_alpha r 0);
          let add = function E.Image ((x, y), img) -> I.add img ~x ~y r in
          List.iter_right add elements;
          r

  let rendered ?transparent c = make (render ?transparent c)
  let map f c = make (f (render c))

  let iter f c =
    let img = render ~fresh:true c in
    f img;
    make img

  let translate dx dy c =
    if dx = 0 && dy = 0 then c
    else { c with elements = List.map (E.translate dx dy) c.elements }

  let viewport ?(x = 0) ?(y = 0) width height c =
    translate (-x) (-y) { c with width; height }

  let bounding_box c =
    let p = (width c, height c) in
    let d = (0, 0) in
    List.fold_left
      (fun (p, d) -> function
        | E.Image ((x, y), img) ->
            (Point.min p (x, y), Point.max d (I.width img, I.height img)))
      (p, d) c.elements

  let scale ?(scaler = I.scale) (nx, dx) (ny, dy) c =
    if nx = dx && ny = dy then c
    else (
      let elements =
        List.map
          (function
            | E.Image ((x, y), img) ->
                let scl =
                  I.create (I.width img * nx / dx) (I.height img * ny / dy)
                in
                scaler img scl;
                E.Image ((x * nx / dx, y * ny / dy), scl))
          c.elements
      in
      { width = c.width; height = c.height; elements })

  let resize ?(proportional = true) ?scaler w' h' img =
    let w = width img in
    let h = height img in
    let (nx, dx), (ny, dy) =
      if proportional then (
        let f = Fraction.min (w', w) (h', h) in
        (f, f))
      else ((w', w), (h', h))
    in
    let x, y =
      if proportional then ((w' - (w * nx / dx)) / 2, (h' - (h * ny / dy)) / 2)
      else (0, 0)
    in
    scale ?scaler (nx, dx) (ny, dy) img |> translate x y |> viewport w' h'

  module Draw = struct
    let line color (x1, y1) (x2, y2) =
      let dx = min x1 x2 in
      let dy = min y1 y2 in
      let w = abs (x2 - x1) in
      let h = abs (y2 - y1) in
      let buf = I.create w h in
      I.blank buf;
      I.fill_alpha buf 0;
      Draw.line
        (fun i j ->
          if 0 <= i && i < w && 0 <= j && j < h then
            I.set_pixel_rgba buf i j color)
        (x1 - dx, y1 - dy)
        (x2 - dx, y2 - dy);
      make ~x:dx ~y:dy ~width:(-1) ~height:(-1) buf
  end
end

module CanvasYUV420 = Canvas (struct
  include ImageYUV420

  let create w h = create w h
  let scale = scale ~proportional:false
end)
