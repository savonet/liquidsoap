(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
open Content_base

module Specs = struct
  open Frame_settings

  type kind = [ `Canvas ]
  type params = { width : int Lazy.t option; height : int Lazy.t option }
  type data = Video.Canvas.t

  let internal_content_type = Some `Video
  let string_of_kind = function `Canvas -> "canvas"
  let json_of_kind = failwith "TODO"

  let make ?(length = 0) (p : params) : data =
    let width = !!(Option.value ~default:video_width p.width) in
    let height = !!(Option.value ~default:video_height p.height) in
    Video.Canvas.make (video_of_main length) (width, height)

  let length d = main_of_video (Video.Canvas.length d)
  let clear _ = ()

  let string_of_params { width; height } =
    print_optional
      [
        ("width", Option.map (fun x -> string_of_int !!x) width);
        ("height", Option.map (fun x -> string_of_int !!x) height);
      ]

  let json_of_params { width; height } =
    [
      Option.map (fun x -> ("width", `Int !!x)) width;
      Option.map (fun x -> ("height", `Int !!x)) height;
    ]
    |> List.filter_map Fun.id

  let parse_param label value =
    match label with
      | "width" ->
          Some { width = Some (lazy (int_of_string value)); height = None }
      | "height" ->
          Some { width = None; height = Some (lazy (int_of_string value)) }
      | _ -> None

  let merge p p' =
    {
      width =
        Option.map Lazy.from_val
          (merge_param ~name:"width"
             (Option.map Lazy.force p.width, Option.map Lazy.force p'.width));
      height =
        Option.map Lazy.from_val
          (merge_param ~name:"height"
             (Option.map Lazy.force p.height, Option.map Lazy.force p'.height));
    }

  let compatible p p' =
    let compare = function
      | None, None -> true
      | Some _, None | None, Some _ -> true
      | Some x, Some y -> !!x = !!y
    in
    compare (p.width, p'.width) && compare (p.height, p'.height)

  let blit src src_pos dst dst_pos len =
    let ( ! ) = Frame_settings.video_of_main in
    Video.Canvas.blit src !src_pos dst !dst_pos !len

  let copy = Video.Canvas.copy

  let params data =
    if Array.length data = 0 then { width = None; height = None }
    else (
      let i = data.(0) in
      {
        width = Some (lazy (Video.Canvas.Image.width i));
        height = Some (lazy (Video.Canvas.Image.height i));
      })

  let kind = `Canvas
  let default_params _ = { width = None; height = None }
  let kind_of_string = function "canvas" -> Some `Canvas | _ -> None
end

include MkContentBase (Specs)

let kind = lift_kind `Canvas

let dimensions_of_format p =
  let p = get_params p in
  let width =
    Lazy.force (Option.value ~default:Frame_settings.video_width p.width)
  in
  let height =
    Lazy.force (Option.value ~default:Frame_settings.video_height p.height)
  in
  (width, height)
