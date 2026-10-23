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

let merge_property ~name ~equal a b =
  match (a, b) with
    | None, v | v, None -> v
    | Some v, Some v' when equal v v' -> Some v
    | _ -> failwith (Printf.sprintf "Incompatible %s" name)

let compatible_property ~equal a b =
  match (a, b) with Some v, Some v' -> equal v v' | _ -> true

let positive_int s =
  match int_of_string_opt s with
    | Some n when n > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s ->
        Some n
    | _ -> None

let print l =
  String.concat ","
    (List.filter_map
       (fun (label, v) -> Option.map (fun v -> label ^ "=" ^ v) v)
       l)

let parse_pairs ~parse_param ~merge ~default = function
  | "" -> Some default
  | s -> (
      let pair p item =
        match String.index_opt item '=' with
          | None -> raise Exit
          | Some i -> (
              let label = String.sub item 0 i in
              let value =
                String.sub item (i + 1) (String.length item - i - 1)
              in
              match parse_param label value with
                | Some p' -> merge p p'
                | None -> raise Exit)
      in
      try Some (List.fold_left pair default (String.split_on_char ',' s))
      with Exit | Failure _ -> None)

module Audio = struct
  type t = {
    channel_layout : Avutil.Channel_layout.t option;
    sample_format : Avutil.Sample_format.t option;
    sample_rate : int option;
  }

  let default =
    { channel_layout = None; sample_format = None; sample_rate = None }

  let equal_layout l l' =
    try Avutil.Channel_layout.compare l l' with _ -> false

  let to_string { channel_layout; sample_format; sample_rate } =
    print
      [
        ( "channel_layout",
          Option.map
            (fun l ->
              try Avutil.Channel_layout.get_description l with _ -> "unknown")
            channel_layout );
        ( "sample_format",
          Option.map
            (fun f ->
              Option.value ~default:"none" (Avutil.Sample_format.get_name f))
            sample_format );
        ("sample_rate", Option.map string_of_int sample_rate);
      ]

  let parse_param label value =
    match label with
      | "channel_layout" -> (
          match Avutil.Channel_layout.find value with
            | l -> Some { default with channel_layout = Some l }
            | exception _ -> None)
      | "sample_format" -> (
          match Avutil.Sample_format.find value with
            | `None -> None
            | f -> Some { default with sample_format = Some f }
            | exception _ -> None)
      | "sample_rate" ->
          Option.map
            (fun r -> { default with sample_rate = Some r })
            (positive_int value)
      | _ -> None

  let compatible p p' =
    compatible_property ~equal:equal_layout p.channel_layout p'.channel_layout
    && compatible_property ~equal:( = ) p.sample_format p'.sample_format
    && compatible_property ~equal:( = ) p.sample_rate p'.sample_rate

  let merge p p' =
    {
      channel_layout =
        merge_property ~name:"channel_layout" ~equal:equal_layout
          p.channel_layout p'.channel_layout;
      sample_format =
        merge_property ~name:"sample_format" ~equal:( = ) p.sample_format
          p'.sample_format;
      sample_rate =
        merge_property ~name:"sample_rate" ~equal:( = ) p.sample_rate
          p'.sample_rate;
    }

  let serialize = to_string
  let parse_serialized = parse_pairs ~parse_param ~merge ~default

  let of_frame frame =
    {
      channel_layout = Some (Avutil.Audio.frame_get_channel_layout frame);
      sample_format = Some (Avutil.Audio.frame_get_sample_format frame);
      sample_rate = Some (Avutil.Audio.frame_get_sample_rate frame);
    }
end

module Video = struct
  type t = {
    width : int option;
    height : int option;
    pixel_format : Avutil.Pixel_format.t option;
    pixel_aspect : Avutil.rational option;
  }

  let default =
    { width = None; height = None; pixel_format = None; pixel_aspect = None }

  let reduce { Avutil.num; den } =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let g = max 1 (gcd (abs num) (abs den)) in
    { Avutil.num = num / g; den = den / g }

  let equal_aspect a a' = reduce a = reduce a'

  let to_string { width; height; pixel_format; pixel_aspect } =
    print
      [
        ("width", Option.map string_of_int width);
        ("height", Option.map string_of_int height);
        ( "pixel_format",
          Option.map
            (fun f ->
              Option.value ~default:"none" (Avutil.Pixel_format.to_string f))
            pixel_format );
        ( "pixel_aspect",
          Option.map
            (fun { Avutil.num; den } -> Printf.sprintf "%d/%d" num den)
            pixel_aspect );
      ]

  let parse_aspect value =
    match String.split_on_char '/' value with
      | [num; den] -> (
          match (positive_int num, positive_int den) with
            | Some num, Some den -> Some { Avutil.num; den }
            | _ -> None)
      | _ -> None

  let parse_param label value =
    match label with
      | "width" ->
          Option.map
            (fun w -> { default with width = Some w })
            (positive_int value)
      | "height" ->
          Option.map
            (fun h -> { default with height = Some h })
            (positive_int value)
      | "pixel_format" -> (
          match Avutil.Pixel_format.of_string value with
            | `None -> None
            | f -> Some { default with pixel_format = Some f }
            | exception _ -> None)
      | "pixel_aspect" ->
          Option.map
            (fun a -> { default with pixel_aspect = Some a })
            (parse_aspect value)
      | _ -> None

  let compatible p p' =
    compatible_property ~equal:( = ) p.width p'.width
    && compatible_property ~equal:( = ) p.height p'.height
    && compatible_property ~equal:( = ) p.pixel_format p'.pixel_format
    && compatible_property ~equal:equal_aspect p.pixel_aspect p'.pixel_aspect

  let merge p p' =
    {
      width = merge_property ~name:"width" ~equal:( = ) p.width p'.width;
      height = merge_property ~name:"height" ~equal:( = ) p.height p'.height;
      pixel_format =
        merge_property ~name:"pixel_format" ~equal:( = ) p.pixel_format
          p'.pixel_format;
      pixel_aspect =
        merge_property ~name:"pixel_aspect" ~equal:equal_aspect p.pixel_aspect
          p'.pixel_aspect;
    }

  let serialize = to_string
  let parse_serialized = parse_pairs ~parse_param ~merge ~default

  let of_frame frame =
    {
      width = Some (Avutil.Video.frame_get_width frame);
      height = Some (Avutil.Video.frame_get_height frame);
      pixel_format = Some (Avutil.Video.frame_get_pixel_format frame);
      pixel_aspect = Avutil.Video.frame_get_pixel_aspect frame;
    }
end
