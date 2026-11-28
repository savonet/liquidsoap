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

open Value

let make params =
  let video_width, video_height =
    (`Setting `Video_width, `Setting `Video_height)
  in
  let defaults =
    {
      Theora_format.bitrate_control = Theora_format.Quality 40;
      fill = None;
      width = video_width;
      height = video_height;
      picture_width = video_width;
      picture_height = video_height;
      picture_x = 0;
      picture_y = 0;
      aspect_numerator = 1;
      aspect_denominator = 1;
      keyframe_frequency = 64;
      vp3_compatible = None;
      soft_target = false;
      buffer_delay = None;
      speed = None;
    }
  in
  let theora =
    List.fold_left
      (fun f -> function
        | `Labelled ("quality", Value.Int { value = i; pos }) ->
            (* According to the doc, this should be a value between
             * 0 and 63. *)
            if i < 0 || i > 63 then
              Format_builders.raise_error ~pos
                "Theora quality should be in 0..63";
            { f with Theora_format.bitrate_control = Theora_format.Quality i }
        | `Labelled ("bitrate", Value.Int { value = i; _ }) ->
            { f with Theora_format.bitrate_control = Theora_format.Bitrate i }
        | `Labelled ("width", Value.Int { value = i; pos }) ->
            (* According to the doc: must be a multiple of 16, and less than 1048576. *)
            if i mod 16 <> 0 || i >= 1048576 then
              Format_builders.raise_error ~pos
                "invalid frame width value (should be a multiple of 16)";
            { f with Theora_format.width = `Value i; picture_width = `Value i }
        | `Labelled ("height", Value.Int { value = i; pos }) ->
            (* According to the doc: must be a multiple of 16, and less than 1048576. *)
            if i mod 16 <> 0 || i >= 1048576 then
              Format_builders.raise_error ~pos
                "invalid frame height value (should be a multiple of 16)";
            {
              f with
              Theora_format.height = `Value i;
              picture_height = `Value i;
            }
        | `Labelled ("picture_width", Value.Int { value = i; _ }) ->
            { f with Theora_format.picture_width = `Value i }
        | `Labelled ("picture_height", Value.Int { value = i; _ }) ->
            { f with Theora_format.picture_height = `Value i }
        | `Labelled ("picture_x", Value.Int { value = i; _ }) ->
            { f with Theora_format.picture_x = i }
        | `Labelled ("picture_y", Value.Int { value = i; _ }) ->
            { f with Theora_format.picture_y = i }
        | `Labelled ("aspect_numerator", Value.Int { value = i; _ }) ->
            { f with Theora_format.aspect_numerator = i }
        | `Labelled ("aspect_denominator", Value.Int { value = i; _ }) ->
            { f with Theora_format.aspect_denominator = i }
        | `Labelled ("keyframe_frequency", Value.Int { value = i; _ }) ->
            { f with Theora_format.keyframe_frequency = i }
        | `Labelled ("vp3_compatible", Bool { value = i }) ->
            { f with Theora_format.vp3_compatible = Some i }
        | `Labelled ("soft_target", Bool { value = i }) ->
            { f with Theora_format.soft_target = i }
        | `Labelled ("buffer_delay", Value.Int { value = i; _ }) ->
            { f with Theora_format.buffer_delay = Some i }
        | `Labelled ("speed", Value.Int { value = i; _ }) ->
            { f with Theora_format.speed = Some i }
        | `Labelled ("bytes_per_page", Value.Int { value = i; _ }) ->
            { f with Theora_format.fill = Some i }
        | t -> Format_builders.raise_generic_error t)
      defaults params
  in
  let known = function `Value v -> Some v | `Setting _ -> None in
  (match Theora_format.picture_error ~known theora with
    | Some (param, message) ->
        let pos =
          List.find_map
            (function
              | `Labelled (l, v) when l = param -> Value.pos v | _ -> None)
            params
        in
        Format_builders.raise_error ~pos message
    | None -> ());
  theora

let () =
  let make p =
    Format_value.Ogg { Ogg_format.audio = None; video = Some (make p) }
  in
  Format_builders.register "theora" make
