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
  include Ffmpeg_content_base

  type kind = [ `Raw ]

  let kind = `Raw
  let string_of_kind = function `Raw -> "ffmpeg.raw"
  let kind_of_string = function "ffmpeg.raw" -> Some `Raw | _ -> None
end

module AudioSpecs = struct
  include BaseSpecs

  type param = {
    channel_layout : Channel_layout.t;
    sample_format : Sample_format.t;
    sample_rate : int;
  }

  type data = (param, audio frame) content

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

  type data = (param, video frame) content

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
