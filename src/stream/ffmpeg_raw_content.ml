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

module Audio = Frame_content.MkContent (AudioSpecs)

module VideoSpecs = struct
  include BaseSpecs

  type param = {
    width : int;
    height : int;
    pixel_format : Avutil.Pixel_format.t option;
  }

  type data = (int * video frame) list ref

  let make _ = ref []
  let clear d = d := []
  let param_of_string _ _ = None

  (* TODO *)
  let bytes data = List.fold_left (fun c _ -> c + 0) 0 !data
  let copy data = ref !data

  let frame_param params =
    {
      width = Video.frame_get_width params;
      height = Video.frame_get_height params;
      pixel_format = Some (Video.frame_get_pixel_format params);
    }

  let mk_param p =
    {
      width = Avcodec.Video.get_width p;
      height = Avcodec.Video.get_height p;
      pixel_format = Avcodec.Video.get_pixel_format p;
    }

  let params c =
    match !c with [] -> [] | (_, frame) :: _ -> [frame_param frame]

  let blit src src_pos dst dst_pos len =
    let src_start = Frame.video_of_master src_pos in
    let src_end = Frame.video_of_master (src_pos + len) in
    List.iter
      (fun (pos, p) ->
        if src_start <= pos && pos < src_end then (
          let pos = dst_pos + (pos - src_pos) in
          dst := (pos, p) :: !dst ))
      !src

  let string_of_kind = function `Raw -> "ffmpeg.raw"
  let kind_of_string = function "ffmpeg.raw" -> Some `Raw | _ -> None

  let string_of_param { width; height; pixel_format } =
    Printf.sprintf "size=\"%dx%d\",pixel_format=%S)" width height
      ( match pixel_format with
        | None -> "unknown"
        | Some pf -> Pixel_format.to_string pf )
end

module Video = Frame_content.MkContent (VideoSpecs)
