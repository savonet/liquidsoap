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

type media =
  [ `Audio of Avutil.audio Avcodec.params
  | `Video of Avutil.video Avcodec.params
  | `Subtitle of Avutil.subtitle Avcodec.params ]

type t = {
  media : media;
  frame_rate : Avutil.rational option;
  time_base : Avutil.rational option;
  pixel_format : Avutil.Pixel_format.t option;
  sample_format : Avutil.Sample_format.t option;
  profile : int option;
  level : int option;
}

let profile_and_level : media -> int option * int option = function
  | `Audio p -> (Avcodec.params_profile p, Avcodec.params_level p)
  | `Video p -> (Avcodec.params_profile p, Avcodec.params_level p)
  | `Subtitle p -> (Avcodec.params_profile p, Avcodec.params_level p)

let make ?frame_rate ?time_base media =
  let profile, level = profile_and_level media in
  let pixel_format, sample_format =
    match media with
      | `Video p -> (Avcodec.Video.get_pixel_format p, None)
      | `Audio p -> (
          match Avcodec.Audio.get_sample_format p with
            | `None -> (None, None)
            | f -> (None, Some f))
      | `Subtitle _ -> (None, None)
  in
  {
    media;
    frame_rate;
    time_base = (match media with `Subtitle _ -> time_base | _ -> None);
    pixel_format;
    sample_format;
    profile;
    level;
  }

let media_name t =
  match t.media with
    | `Audio _ -> "audio"
    | `Video _ -> "video"
    | `Subtitle _ -> "subtitle"

let codec_name t =
  match t.media with
    | `Audio p -> Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id p)
    | `Video p -> Avcodec.Video.string_of_id (Avcodec.Video.get_params_id p)
    | `Subtitle p ->
        Avcodec.Subtitle.string_of_id (Avcodec.Subtitle.get_params_id p)

let descriptor t =
  match t.media with
    | `Audio p -> Avcodec.descriptor p
    | `Video p -> Avcodec.descriptor p
    | `Subtitle p -> Avcodec.descriptor p

let intra_only t =
  match t.media with
    | `Subtitle _ | `Audio _ -> true
    | _ -> (
        match descriptor t with
          | None -> true
          | Some { Avcodec.properties } -> List.mem `Intra_only properties)

let has_reordering t =
  match t.media with
    | `Video p -> Avcodec.Video.get_video_delay p > 0
    | _ -> false

let extradata t =
  match t.media with
    | `Audio p -> Avcodec.params_extradata p
    | `Video p -> Avcodec.params_extradata p
    | `Subtitle p -> Avcodec.params_extradata p

let starts_with_start_code s =
  let prefix p =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in
  prefix "\000\000\001" || prefix "\000\000\000\001"

let bitstream_form t =
  match t.media with
    | `Video p -> (
        match Avcodec.Video.get_params_id p with
          | `H264 | `Hevc | `Vvc ->
              let e = extradata t in
              if e = "" || starts_with_start_code e then "annexb"
              else "length_prefixed"
          | _ -> "none")
    | `Audio p -> (
        match Avcodec.Audio.get_params_id p with
          | `Aac -> if extradata t = "" then "in_band" else "raw"
          | _ -> "none")
    | `Subtitle _ -> "none"

let relaxed () = Settings.relaxed_compatibility_check#get

let pixel_format_name = function
  | None -> "none"
  | Some f -> Option.value ~default:"none" (Avutil.Pixel_format.to_string f)

let sample_format_name = function
  | None -> "none"
  | Some f -> Option.value ~default:"none" (Avutil.Sample_format.get_name f)

let layout_name l =
  try Avutil.Channel_layout.get_description l with _ -> "unknown"

let rational_to_string { Avutil.num; den } = Printf.sprintf "%d/%d" num den

let differ name show a b equal =
  if equal a b then None else Some (name, show a, show b)

let both_set equal a b =
  match (a, b) with Some a, Some b -> equal a b | _ -> true

let type_differences t t' =
  let codec = differ "codec" Fun.id (codec_name t) (codec_name t') ( = ) in
  let base =
    [
      differ "media" Fun.id (media_name t) (media_name t') ( = );
      codec;
      differ "bitstream form" Fun.id (bitstream_form t) (bitstream_form t')
        ( = );
    ]
  in
  let media =
    match (t.media, t'.media) with
      | `Audio p, `Audio p' ->
          [
            differ "sample rate" string_of_int
              (Avcodec.Audio.get_sample_rate p)
              (Avcodec.Audio.get_sample_rate p')
              ( = );
            differ "channel layout" layout_name
              (Avcodec.Audio.get_channel_layout p)
              (Avcodec.Audio.get_channel_layout p') (fun l l' ->
                try Avutil.Channel_layout.compare l l' with _ -> false);
            differ "sample format" sample_format_name t.sample_format
              t'.sample_format (both_set ( = ));
          ]
      | `Video p, `Video p' ->
          [
            differ "width" string_of_int
              (Avcodec.Video.get_width p)
              (Avcodec.Video.get_width p')
              ( = );
            differ "height" string_of_int
              (Avcodec.Video.get_height p)
              (Avcodec.Video.get_height p')
              ( = );
            differ "pixel format" pixel_format_name t.pixel_format
              t'.pixel_format (both_set ( = ));
          ]
      | _ -> []
  in
  if relaxed () then List.filteri (fun i _ -> i < 2) base else base @ media

let reduce { Avutil.num; den } =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  match (num, den) with
    | 0, _ | _, 0 -> { Avutil.num = 1; den = 1 }
    | _ ->
        let g = gcd (abs num) (abs den) in
        { Avutil.num = num / g; den = den / g }

let to_string t =
  let codec = Printf.sprintf "codec=%S" (codec_name t) in
  match t.media with
    | `Audio p ->
        String.concat ","
          [
            codec;
            Printf.sprintf "channel_layout=%S"
              (layout_name (Avcodec.Audio.get_channel_layout p));
            Printf.sprintf "sample_format=%s"
              (sample_format_name t.sample_format);
            Printf.sprintf "sample_rate=%d" (Avcodec.Audio.get_sample_rate p);
          ]
    | `Video p ->
        String.concat ","
          ([
             codec;
             Printf.sprintf "width=%d" (Avcodec.Video.get_width p);
             Printf.sprintf "height=%d" (Avcodec.Video.get_height p);
             Printf.sprintf "aspect_ratio=%s"
               (rational_to_string
                  (reduce (Avcodec.Video.get_sample_aspect_ratio p)));
           ]
          @ (match t.frame_rate with
            | Some r -> [Printf.sprintf "framerate=%s" (rational_to_string r)]
            | None -> [])
          @
            match t.pixel_format with
            | Some _ ->
                [
                  Printf.sprintf "pixel_format=%s"
                    (pixel_format_name t.pixel_format);
                ]
            | None -> [])
    | `Subtitle _ ->
        String.concat ","
          ([codec]
          @
            match t.time_base with
            | Some tb -> [Printf.sprintf "time_base=%s" (rational_to_string tb)]
            | None -> [])

let incompatibility t t' =
  List.find_map Fun.id (type_differences t t')
  |> Option.map (fun (name, v, v') ->
      Printf.sprintf "%s differs (%s against %s)" name v v')

let compatible t t' = incompatibility t t' = None

let merge t t' =
  (match incompatibility t t' with
    | Some reason ->
        failwith
          (Printf.sprintf "Incompatible copy parameters: %s: %s and %s" reason
             (to_string t) (to_string t'))
    | None -> ());
  let fill a b = match a with Some _ -> a | None -> b in
  {
    t with
    frame_rate = fill t.frame_rate t'.frame_rate;
    pixel_format = fill t.pixel_format t'.pixel_format;
    sample_format = fill t.sample_format t'.sample_format;
    profile = fill t.profile t'.profile;
    level = fill t.level t'.level;
  }

let block_align_and_bits t =
  match t.media with
    | `Audio p ->
        (Avcodec.params_block_align p, Avcodec.params_bits_per_coded_sample p)
    | `Video p ->
        (Avcodec.params_block_align p, Avcodec.params_bits_per_coded_sample p)
    | `Subtitle p ->
        (Avcodec.params_block_align p, Avcodec.params_bits_per_coded_sample p)

let header_compatible t t' =
  relaxed ()
  || extradata t = extradata t'
     && both_set ( = ) t.profile t'.profile
     && both_set ( = ) t.level t'.level
     && block_align_and_bits t = block_align_and_bits t'

let descriptive_side_data =
  [
    `Displaymatrix;
    `Stereo3d;
    `Spherical;
    `Mastering_display_metadata;
    `Content_light_level;
    `Dovi_conf;
    `Icc_profile;
  ]

let side_data_differences p p' =
  let entries p =
    List.filter
      (fun { Avcodec.Packet_side_data.kind } ->
        List.mem kind descriptive_side_data)
      (Avcodec.params_side_data p)
  in
  let e = entries p and e' = entries p' in
  List.filter_map
    (fun kind ->
      let data l =
        List.find_map
          (fun { Avcodec.Packet_side_data.kind = k; data } ->
            if k = kind then Some data else None)
          l
      in
      let show = function
        | None -> "none"
        | Some d -> Digest.(to_hex (string d))
      in
      differ (Avcodec.Packet_side_data.name kind) show (data e) (data e') ( = ))
    descriptive_side_data

let field_order_name = function
  | `Unknown -> "unknown"
  | `Progressive -> "progressive"
  | `Tt -> "tt"
  | `Bb -> "bb"
  | `Tb -> "tb"
  | `Bt -> "bt"

let descriptive_differences t t' =
  match (t.media, t'.media) with
    | `Video p, `Video p' ->
        let c = Avcodec.Video.get_color p and c' = Avcodec.Video.get_color p' in
        List.filter_map Fun.id
          [
            differ "aspect ratio" rational_to_string
              (reduce (Avcodec.Video.get_sample_aspect_ratio p))
              (reduce (Avcodec.Video.get_sample_aspect_ratio p'))
              ( = );
            differ "field order" field_order_name
              (Avcodec.Video.get_field_order p)
              (Avcodec.Video.get_field_order p')
              ( = );
            differ "colour range" Avutil.Color_range.name c.color_range
              c'.color_range ( = );
            differ "colour primaries" Avutil.Color_primaries.name
              c.color_primaries c'.color_primaries ( = );
            differ "colour transfer" Avutil.Color_trc.name c.color_trc
              c'.color_trc ( = );
            differ "colour space" Avutil.Color_space.name c.color_space
              c'.color_space ( = );
            differ "chroma location" Avutil.Chroma_location.name
              c.chroma_location c'.chroma_location ( = );
          ]
        @ side_data_differences p p'
    | _ -> []

let equal t t' =
  t == t' || (to_string t = to_string t' && extradata t = extradata t')
