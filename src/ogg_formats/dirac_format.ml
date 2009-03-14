(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Schroedinger

let create_encoder ~quality ~metadata () =
  let frame_x = Fmt.video_width () in
  let frame_y = Fmt.video_height () in
  (* Using Yuv420 *)
  let video_format = get_default_video_format CUSTOM in
  let video_format =
    {
     video_format with
      width = frame_x;
      height = frame_y;
      frame_rate_numerator = Fmt.video_frames_of_seconds 1.;
      frame_rate_denominator = 1;
      aspect_ratio_numerator = 1;
      aspect_ratio_denominator = 1
    }
  in
  let enc = Encoder.create video_format in
  let started = ref false in
  let header_encoder os = 
    Encoder.encode_header enc os;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os = 
    None
  in
  let stream_start os = "" in
  let ((y,y_stride), (u, v, uv_stride) as yuv) =
    RGB.create_yuv (Fmt.video_width ()) (Fmt.video_height ())
  in
  let dirac_yuv =
  {
    planes = [|(y,y_stride);(u,uv_stride);(v,uv_stride)|];
    frame_width = Fmt.video_width ();
    frame_height = Fmt.video_height ();
    format = Yuv_420_p
  }
  in
  let convert =
    Video_converter.find_converter
      (Video_converter.RGB Video_converter.Rgba_32)
      (Video_converter.YUV Video_converter.Yuvj_420)
  in
  let data_encoder ogg_enc data os = 
    if not !started then
      started := true;
    let b,ofs,len = data.Ogg_encoder.data,data.Ogg_encoder.offset,
                    data.Ogg_encoder.length 
    in
    for i = ofs to ofs+len-1 do
      let frame = Video_converter.frame_of_internal_rgb b.(0).(i) in
      convert
        frame (* TODO: multiple channels.. *)
        (Video_converter.frame_of_internal_yuv
        (Fmt.video_width ())
        (Fmt.video_height ())
             yuv); (* TODO: custom video size.. *);
      Encoder.encode_frame enc dirac_yuv os
    done
  in
  let end_of_page p =
    frames_of_granulepos (Ogg.Page.granulepos p)
  in 
  let end_of_stream os =
    (* Encode at least some data.. *)
    if not !started then
     begin
      RGB.blank_yuv yuv;
      Encoder.encode_frame enc dirac_yuv os
     end;
    Encoder.eos enc os
  in
  {
   Ogg_encoder.
    header_encoder = header_encoder;
    fisbone_packet = fisbone_packet;
    stream_start   = stream_start;
    data_encoder   = (Ogg_encoder.Video_encoder data_encoder);
    (* TODO: dirac output with different rate than the global
     * ones.. *)
    rate           = Fmt.ticks_of_video_frames 1;
    end_of_page    = end_of_page;
    end_of_stream  = end_of_stream
  }
