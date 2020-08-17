(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Rawright 2003-2019 Savonet team

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

open Avutil

module BaseSpecs = struct
  type kind = [ `Raw ]

  let kind = `Raw
  let default_params _ = []

  let merge l l' =
    match (l, l') with
      | [], [p'] -> [p']
      | [p], [] -> [p]
      | [p], [p'] when p = p' -> [p]
      | _ -> failwith "Incompatible parameters"
end

module AudioSpecs = struct
  include BaseSpecs

  type param = {
    channel_layout : Channel_layout.t;
    sample_format : Sample_format.t;
    sample_rate : int;
  }

  type data = audio frame

  let make = function
    | [{ channel_layout; sample_format; sample_rate }] ->
        Audio.create_frame sample_format channel_layout sample_rate
          Frame.(audio_of_master (Lazy.force Frame.size))
    | _ -> assert false

  let clear _ = ()
  let param_of_string _ _ = None

  (* TODO *)
  let bytes _ = 0

  let frame_param frame =
    {
      channel_layout = Audio.frame_get_channel_layout frame;
      sample_format = Audio.frame_get_sample_format frame;
      sample_rate = Audio.frame_get_sample_rate frame;
    }

  let mk_param p =
    {
      channel_layout = Avcodec.Audio.get_channel_layout p;
      sample_format = Avcodec.Audio.get_sample_format p;
      sample_rate = Avcodec.Audio.get_sample_rate p;
    }

  let copy src =
    let dst = make [frame_param src] in
    frame_copy src dst;
    dst

  let params frame = [frame_param frame]

  let blit src src_pos dst dst_pos len =
    let src_pos = Frame.audio_of_master src_pos in
    let dst_pos = Frame.audio_of_master dst_pos in
    let len = Frame.audio_of_master len in
    Audio.frame_copy_samples src src_pos dst dst_pos len

  let string_of_kind = function `Raw -> "ffmpeg.raw"
  let kind_of_string = function "ffmpeg.raw" -> Some `Raw | _ -> None

  let string_of_param { channel_layout; sample_format; sample_rate } =
    Printf.sprintf "channel_layout=%S,sample_format=%S,sample_rate=%d"
      (Channel_layout.get_description channel_layout)
      (Sample_format.get_name sample_format)
      sample_rate
end

module Audio = struct
  include Frame_content.MkContent (AudioSpecs)

  let internal_params () =
    {
      AudioSpecs.channel_layout =
        Avutil.Channel_layout.get_default (Lazy.force Frame.audio_channels);
      sample_format = `Dbl;
      sample_rate = Lazy.force Frame.audio_rate;
    }
end

module VideoSpecs = struct
  include BaseSpecs

  type param = {
    width : int;
    height : int;
    pixel_format : Avutil.Pixel_format.t option;
  }

  type data = { param : param; mutable data : (int * video frame) list }

  let make = function [param] -> { param; data = [] } | _ -> assert false
  let clear d = d.data <- []
  let param_of_string _ _ = None

  let blit src src_pos dst dst_pos len =
    let src_end = src_pos + len in
    let data =
      List.fold_left
        (fun data (pos, p) ->
          if src_pos <= pos && pos < src_end then (
            let pos = dst_pos + (pos - src_pos) in
            (pos, p) :: data )
          else data)
        dst.data src.data
    in
    dst.data <- data

  let copy { data; param } = { data; param }

  (* TODO *)
  let bytes _ = 0

  let frame_param frame =
    {
      width = Video.frame_get_width frame;
      height = Video.frame_get_height frame;
      pixel_format = Some (Video.frame_get_pixel_format frame);
    }

  let mk_param p =
    {
      width = Avcodec.Video.get_width p;
      height = Avcodec.Video.get_height p;
      pixel_format = Avcodec.Video.get_pixel_format p;
    }

  let params { param } = [param]
  let string_of_kind = function `Raw -> "ffmpeg.raw"
  let kind_of_string = function "ffmpeg.raw" -> Some `Raw | _ -> None

  let string_of_param { width; height; pixel_format } =
    Printf.sprintf "size=\"%dx%d\",pixel_format=%S)" width height
      ( match pixel_format with
        | None -> "unknown"
        | Some pf -> Pixel_format.to_string pf )
end

module Video = struct
  include Frame_content.MkContent (VideoSpecs)

  let internal_params () =
    {
      VideoSpecs.width = Lazy.force Frame.video_width;
      height = Lazy.force Frame.video_height;
      pixel_format = Some `Yuv420p;
    }
end
