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

open Mm

let pack image =
  let y = (Image.YUV420.y image, Image.YUV420.y_stride image) in
  let u = (Image.YUV420.u image, Image.YUV420.uv_stride image) in
  let v = (Image.YUV420.v image, Image.YUV420.uv_stride image) in
  match Image.YUV420.alpha image with
    | Some a -> [| y; u; v; (a, Image.YUV420.y_stride image) |]
    | None -> [| y; u; v |]

let unpack ~width ~height planes =
  match planes with
    | [| (y, ys); (u, us); (v, vs) |] when us = vs ->
        Image.YUV420.make width height y ys u v us
    | [| (y, ys); (u, us); (v, vs); (a, as_) |] when us = vs && as_ = ys ->
        Image.YUV420.make width height ~alpha:a y ys u v us
    | _ ->
        failwith
          (Printf.sprintf "Cannot unpack %d planes with strides %s"
             (Array.length planes)
             (String.concat ", "
                (Array.to_list
                   (Array.map (fun (_, s) -> string_of_int s) planes))))
