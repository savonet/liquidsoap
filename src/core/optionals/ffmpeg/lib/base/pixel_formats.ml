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

let has_alpha format =
  try List.mem `Alpha (Avutil.Pixel_format.descriptor format).flags
  with _ -> false

let is_hardware format =
  try List.mem `Hwaccel (Avutil.Pixel_format.descriptor format).flags
  with _ -> false

let internal_for format = if has_alpha format then `Yuva420p else `Yuv420p

let for_encoder ?configured ~alpha codec =
  match configured with
    | Some name -> (
        match Avutil.Pixel_format.of_string name with
          | format -> format
          | exception _ ->
              failwith (Printf.sprintf "Unknown pixel format: %s" name))
    | None ->
        let supported = Avcodec.Video.get_supported_pixel_formats codec in
        let preferred = if alpha then `Yuva420p else `Yuv420p in
        if List.mem preferred supported then preferred
        else (
          match
            List.find_opt
              (fun f -> (not (is_hardware f)) && ((not alpha) || has_alpha f))
              supported
          with
            | Some f -> f
            | None -> `Yuv420p)

let same_video a b =
  let ignore =
    (if Settings.format_change_color#get then [] else [`Color])
    @ if Settings.format_change_pixel_aspect#get then [] else [`Pixel_aspect]
  in
  Avutil.Video.same_frame_format ~ignore a b

let name_or_unknown = function Some n -> n | None -> "unknown"

let describe_video (f : Avutil.Video.frame_format) =
  Printf.sprintf
    "%dx%d %s, pixel aspect %s, colour space %s, range %s, primaries %s, \
     transfer %s, chroma location %s"
    f.width f.height
    (name_or_unknown (Avutil.Pixel_format.to_string f.pixel_format))
    (match f.pixel_aspect with
      | Some { Avutil.num; den } -> Printf.sprintf "%d/%d" num den
      | None -> "unknown")
    (Avutil.Color_space.name f.color_space)
    (Avutil.Color_range.name f.color_range)
    (Avutil.Color_primaries.name f.color_primaries)
    (Avutil.Color_trc.name f.color_trc)
    (Avutil.Chroma_location.name f.chroma_location)

let describe_audio (f : Avutil.Audio.frame_format) =
  Printf.sprintf "%s, %d Hz, %s"
    (name_or_unknown (Avutil.Sample_format.get_name f.sample_format))
    f.sample_rate
    (try Avutil.Channel_layout.get_description f.channel_layout
     with _ -> "unknown")

let scaling_flag () : Swscale.flag =
  match Settings.scaling_algorithm#get with
    | "fast_bilinear" -> Fast_bilinear
    | "bilinear" -> Bilinear
    | _ -> Bicubic
