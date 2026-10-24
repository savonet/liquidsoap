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

type t = [ `Empty | `Converter of Avfilter.Utils.display_converter ]

let on_undecided = ref None

let cropping params =
  Avcodec.Packet_side_data.cropping (Avcodec.params_side_data params)

let ignore_properties () =
  (if Settings.format_change_color#get then [] else [`Color])
  @ if Settings.format_change_pixel_aspect#get then [] else [`Pixel_aspect]

let create ?params ~time_base () : t =
  if not Settings.autorotate#get then `Empty
  else
    `Converter
      (Avfilter.Utils.init_display_converter
         ?cropping:(Option.bind params cropping)
         ?on_undecided:!on_undecided ~ignore:(ignore_properties ()) ~time_base
         ())

let convert (t : t) deliver frame =
  match t with
    | `Empty -> deliver frame
    | `Converter c -> Avfilter.Utils.convert_display c deliver (`Frame frame)

let eof (t : t) deliver =
  match t with
    | `Empty -> ()
    | `Converter c -> Avfilter.Utils.convert_display c deliver `Flush

let displayed ?stored params =
  let width = Avcodec.Video.get_width params in
  let height = Avcodec.Video.get_height params in
  let pixel_aspect =
    match stored with
      | Some _ -> stored
      | None -> Avcodec.Video.get_pixel_aspect params
  in
  let stored_layout = (width, height, pixel_aspect) in
  if not Settings.autorotate#get then stored_layout
  else (
    match
      Avfilter.Utils.display_layout ?cropping:(cropping params)
        ?display_matrix:
          (Avcodec.Packet_side_data.display_matrix
             (Avcodec.params_side_data params))
        ?pixel_aspect ~width ~height ()
    with
      | `Layout { width; height; pixel_aspect } -> (width, height, pixel_aspect)
      | `Undecided _ -> stored_layout)

let expected_size params =
  let w, h, _ = displayed params in
  (w, h)

let pixel_aspect ?stored params =
  let _, _, a = displayed ?stored params in
  a
