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

let check = Theora.Decoder.check

let decoder os =
  let decoder = ref None in
  let meta    = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
  let packet3 = ref None in
  let fill feed =
    (* Decoder is created upon first decoding..*)
    let decoder,fps =
      match !decoder with
        | None ->
           let packet1 =
             match !packet1 with
               | None ->
                  let p = Ogg.Stream.get_packet os in
                  packet1 := Some p; p
               | Some p -> p
           in
           let packet2 =
             match !packet2 with
               | None ->
                  let p = Ogg.Stream.get_packet os in
                  packet2 := Some p; p
               | Some p -> p
           in
           let packet3 =
             match !packet3 with
               | None ->
                   let p = Ogg.Stream.get_packet os in
                   packet3 := Some p; p
               | Some p -> p
           in
           let (d,info,vendor,m) = Theora.Decoder.create packet1 packet2 packet3 in
           let fps = (float (info.Theora.fps_numerator)) /.
                     (float (info.Theora.fps_denominator))
           in
           meta := Some (vendor,m);
           decoder := Some (d,fps);
           d,fps
        | Some d -> d
    in
    let ret = Theora.Decoder.get_yuv decoder os in
    let ret =
    {
      Ogg_demuxer.
        y_width   = ret.Theora.y_width;
        y_height  = ret.Theora.y_height;
        y_stride  = ret.Theora.y_stride;
        uv_width  = ret.Theora.uv_width;
        uv_height = ret.Theora.uv_height;
        uv_stride = ret.Theora.uv_stride;
        fps       = fps;
        y = ret.Theora.y;
        u = ret.Theora.u;
        v = ret.Theora.v
    }
    in
    let m = ! meta in
    meta := None;
    feed (ret,m)
  in
  Ogg_demuxer.Video fill

let () = Ogg_demuxer.ogg_decoders#register "theora" (check,decoder)

let create_encoder ~quality ~metadata () =
  let frame_x = Fmt.video_width () in
  let frame_y = Fmt.video_height () in
  (* Theora has a divisible-by-sixteen restriction for the encoded video size. *)
  (* Scale the frame size up to the nearest /16 and calculate offsets. *)
  let video_x = ((frame_x + 15) lsr 4) lsl 4 in
  let video_y = ((frame_y + 15) lsr 4) lsl 4 in
  let frame_x_offset = ((video_x - frame_x) / 2) land (lnot 1) in
  let frame_y_offset = ((video_y - frame_y) / 2) land (lnot 1) in
  let video_r = 800 in
  (* TODO: variable FPS *)
  let fps = Fmt.video_frames_of_seconds 1. in
  let info =
    {
     Theora.
      width = video_x;
      height = video_y;
      frame_width = frame_x;
      frame_height = frame_y;
      offset_x = frame_x_offset;
      offset_y = frame_y_offset;
      fps_numerator = fps;
      fps_denominator = 1;
      aspect_numerator = 1;
      aspect_denominator = 1;
      colorspace = Theora.CS_unspecified;
      target_bitrate = video_r;
      quality = quality;
      quick_p = true;
      version_major = 0;
      version_minor = 0;
      version_subminor = 0;
      dropframes_p = false;
      keyframe_auto_p = true;
      keyframe_frequency = 64;
      keyframe_frequency_force = 64;
      keyframe_data_target_bitrate = (video_r * 3 / 2);
      keyframe_auto_threshold = 80;
      keyframe_mindistance = 8;
      noise_sensitivity = 1;
      sharpness = 1; (* ??? *)
      pixelformat = Theora.PF_420
    }
  in
  let enc = Theora.Encoder.create info in
  let started = ref false in
  let header_encoder os = 
    Theora.Encoder.encode_header enc os;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os = 
    let serialno = Ogg.Stream.serialno os in
    Some (Theora.Skeleton.fisbone ~serialno ~info ())
  in
  let stream_start os = 
    Theora.Encoder.encode_comments os metadata;
    Theora.Encoder.encode_tables enc os;
    Ogg_encoder.flush_pages os
  in
  let ((y,y_stride), (u, v, uv_stride) as yuv) =
    RGB.create_yuv (Fmt.video_width ()) (Fmt.video_height ())
  in
  let theora_yuv =
  {
    Theora.y_width = Fmt.video_width ();
    Theora.y_height = Fmt.video_height ();
    Theora.y_stride = y_stride;
    Theora.uv_width = Fmt.video_width () / 2;
    Theora.uv_height = Fmt.video_height () / 2;
    Theora.uv_stride = uv_stride;
    Theora.y = y;
    Theora.u = u;
    Theora.v = v;
  }
  in
  let convert =
    Video_converter.find_converter
      (Video_converter.RGB Video_converter.Rgba_32)
      (Video_converter.YUV Video_converter.Yuvj_420)
  in
  let data_encoder ogg_enc data os _ = 
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
      Theora.Encoder.encode_buffer enc os theora_yuv 
    done
  in
  let end_of_page p = 
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then
      Ogg_encoder.Unknown
    else
      if granulepos <> Int64.zero then
       let index = 
         Int64.succ (Theora.Encoder.frames_of_granulepos enc granulepos)
       in
       Ogg_encoder.Time (Int64.to_float index /. (float fps)) 
      else
       Ogg_encoder.Time 0.
  in
  let end_of_stream os =
    (* Encode at least some data.. *)
    if not !started then
     begin
      RGB.blank_yuv yuv;
      Theora.Encoder.encode_buffer enc os theora_yuv
     end;
    Theora.Encoder.eos enc os
  in
  {
   Ogg_encoder.
    header_encoder = header_encoder;
    fisbone_packet = fisbone_packet;
    stream_start   = stream_start;
    data_encoder   = (Ogg_encoder.Video_encoder data_encoder);
    end_of_page    = end_of_page;
    end_of_stream  = end_of_stream
  }
