(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

type audio_params = {
  channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.t;
}

type video_params = { width : int Lazy.t option; height : int Lazy.t option }
type midi_params = { channels : int }

module AudioSpecs = struct
  open Frame_settings
  open Contents

  type kind = [ `Pcm ]
  type params = audio_params
  type data = Audio.Mono.buffer array

  let internal_content_type = Some `Audio
  let string_of_kind = function `Pcm -> "pcm"

  let string_of_params { channel_layout } =
    match !!channel_layout with
      | `Mono -> "mono"
      | `Stereo -> "stereo"
      | `Five_point_one -> "5.1"

  let merge p p' =
    assert (!!(p.channel_layout) = !!(p'.channel_layout));
    p

  let compatible p p' = !!(p.channel_layout) = !!(p'.channel_layout)

  let blit src src_pos dst dst_pos len =
    let ( ! ) = audio_of_main in
    Audio.blit src !src_pos dst !dst_pos !len

  let copy d = Audio.copy d 0 (Audio.length d)

  let param_of_channels = function
    | 1 -> { channel_layout = lazy `Mono }
    | 2 -> { channel_layout = lazy `Stereo }
    | 6 -> { channel_layout = lazy `Five_point_one }
    | _ -> raise Invalid

  let channels_of_param = function
    | `Mono -> 1
    | `Stereo -> 2
    | `Five_point_one -> 6

  let parse_param label value =
    match (label, value) with
      | "", "mono" -> Some { channel_layout = lazy `Mono }
      | "", "stereo" -> Some { channel_layout = lazy `Stereo }
      | "", "5.1" -> Some { channel_layout = lazy `Five_point_one }
      | _ -> None

  let params d = param_of_channels (Array.length d)
  let kind = `Pcm

  let default_params _ =
    param_of_channels (Lazy.force Frame_settings.audio_channels)

  let clear _ = ()

  let make ~length { channel_layout } =
    let channels =
      match !!channel_layout with
        | `Mono -> 1
        | `Stereo -> 2
        | `Five_point_one -> 6
    in
    Array.init channels (fun _ -> Audio.Mono.create (audio_of_main length))

  let length d = main_of_audio (Audio.length d)
  let kind_of_string = function "audio" | "pcm" -> Some `Pcm | _ -> None
end

module Audio = struct
  open Contents
  include MkContent (AudioSpecs)

  let kind = lift_kind `Pcm

  let format_of_channels = function
    | 1 -> lift_params { channel_layout = lazy `Mono }
    | 2 -> lift_params { channel_layout = lazy `Stereo }
    | 6 -> lift_params { channel_layout = lazy `Five_point_one }
    | _ -> raise Invalid

  let channels_of_format p =
    AudioSpecs.(channels_of_param (Lazy.force (get_params p).channel_layout))
end

module VideoSpecs = struct
  open Frame_settings
  open Contents

  type kind = [ `Canvas ]
  type params = video_params
  type data = Video.Canvas.t

  let internal_content_type = Some `Video
  let string_of_kind = function `Canvas -> "canvas"

  let make ~length (p : params) : data =
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

module Video = struct
  include MkContent (VideoSpecs)

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
end

module MidiSpecs = struct
  open Frame_settings
  open Contents

  type kind = [ `Midi ]
  type params = midi_params
  type data = MIDI.Multitrack.t

  let internal_content_type = Some `Midi
  let string_of_kind = function `Midi -> "midi"
  let string_of_params { channels } = Printf.sprintf "channels=%d" channels

  let merge p p' =
    assert (p.channels = p'.channels);
    p

  let compatible p p' = p.channels = p'.channels

  let blit src src_pos dst dst_pos len =
    let ( ! ) = midi_of_main in
    Array.iter2 (fun m m' -> MIDI.blit m !src_pos m' !dst_pos !len) src dst

  let copy m = Array.map MIDI.copy m
  let params m = { channels = MIDI.Multitrack.channels m }
  let kind = `Midi
  let default_params _ = { channels = Lazy.force Frame_settings.midi_channels }
  let clear _ = ()

  let make ~length { channels } =
    MIDI.Multitrack.create channels (midi_of_main length)

  let length d = main_of_midi (MIDI.Multitrack.duration d)
  let kind_of_string = function "midi" -> Some `Midi | _ -> None

  let parse_param label value =
    match (label, value) with
      | "channels", c -> Some { channels = int_of_string c }
      | _ | (exception _) -> None
end

module Midi = struct
  include MkContent (MidiSpecs)

  let kind = lift_kind `Midi
end

let default_audio () =
  let channels = Lazy.force Frame_settings.audio_channels in
  if channels = 0 then None.format else Audio.format_of_channels channels

let default_video () =
  if Lazy.force Frame_settings.default_video_enabled then
    Video.lift_params
      {
        width = Some Frame_settings.video_width;
        height = Some Frame_settings.video_height;
      }
  else None.format

let default_midi () =
  let channels = Lazy.force Frame_settings.midi_channels in
  if channels = 0 then None.format else Midi.lift_params { channels }
