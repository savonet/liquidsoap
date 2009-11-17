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

let create_encoder ~quality ~metadata () =
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  (* Theora has a divisible-by-sixteen restriction for the encoded video size. *)
  (* Scale the frame size up to the nearest /16 and calculate offsets. *)
  let video_x = ((width + 15) lsr 4) lsl 4 in
  let video_y = ((height + 15) lsr 4) lsl 4 in
  let frame_x_offset = ((video_x - width) / 2) land (lnot 1) in
  let frame_y_offset = ((video_y - height) / 2) land (lnot 1) in
  let video_r = 800 in
  (* TODO: variable FPS *)
  let fps = Frame.video_of_seconds 1. in
  let info =
    {
     Theora.
      width = video_x;
      height = video_y;
      frame_width = width;
      frame_height = height;
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
    Ogg_muxer.flush_pages os
  in
  let ((y,y_stride), (u, v, uv_stride) as yuv) =
    RGB.create_yuv width height
  in
  let theora_yuv =
  {
    Theora.y_width = width ;
    Theora.y_height = height ;
    Theora.y_stride = y_stride;
    Theora.uv_width = width / 2;
    Theora.uv_height = height / 2;
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
  let data_encoder data os _ = 
    if not !started then
      started := true;
    let b,ofs,len = data.Ogg_muxer.data,data.Ogg_muxer.offset,
                    data.Ogg_muxer.length 
    in
    for i = ofs to ofs+len-1 do
      let frame = Video_converter.frame_of_internal_rgb b.(0).(i) in
      convert
        frame
        (Video_converter.frame_of_internal_yuv
         width height yuv); (* TODO: custom video size.. *)
      Theora.Encoder.encode_buffer enc os theora_yuv 
    done
  in
  let end_of_page p = 
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then
      Ogg_muxer.Unknown
    else
      if granulepos <> Int64.zero then
       let index = 
         Int64.succ (Theora.Encoder.frames_of_granulepos enc granulepos)
       in
       Ogg_muxer.Time (Int64.to_float index /. (float fps)) 
      else
       Ogg_muxer.Time 0.
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
   Ogg_muxer.
    header_encoder = header_encoder;
    fisbone_packet = fisbone_packet;
    stream_start   = stream_start;
    data_encoder   = (Ogg_muxer.Video_encoder data_encoder);
    end_of_page    = end_of_page;
    end_of_stream  = end_of_stream
  }

let create_theora = 
  function 
    | Encoder.Ogg.Theora theora -> 
       let quality = theora.Encoder.Theora.quality in
       let reset ogg_enc metadata =
         let f l v cur = (l,v) :: cur in
         let metadata = Hashtbl.fold f metadata [] in 
         let enc =
           create_encoder
              ~quality ~metadata ()
         in
         Ogg_muxer.register_track ogg_enc enc
       in
       { 
        Ogg_encoder.
           encode = Ogg_encoder.encode_video ;
           reset  = reset  ;
           id     = None
       }
    | _ -> assert false

let () = Hashtbl.add Ogg_encoder.encoders "theora" create_theora
