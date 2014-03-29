(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2014 Savonet team

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

module Img = Image.Generic
module P = Img.Pixel

let create_encoder ~metadata dirac =
  let width = Lazy.force dirac.Encoder.Dirac.width in
  let height = Lazy.force dirac.Encoder.Dirac.height in
  let aspect_numerator = dirac.Encoder.Dirac.aspect_numerator in
  let aspect_denominator = dirac.Encoder.Dirac.aspect_denominator in
  let quality = dirac.Encoder.Dirac.quality in
  (* TODO: variable fps *)
  let fps = Frame.video_of_seconds 1. in
  (* Using Yuv420 *)
  let video_format = 
    Schroedinger.get_default_video_format Schroedinger.CUSTOM 
  in
  let video_format =
    {
     video_format with
      Schroedinger.
       width = width;
       height = height;
       clean_width = width;
       clean_height = height;
       left_offset = 0;
       top_offset = 0;
       colour_matrix = Schroedinger.HDTV;
       signal_range = Schroedinger.RANGE_8BIT_VIDEO;
       frame_rate_numerator = fps;
       frame_rate_denominator = 1;
       aspect_ratio_numerator = aspect_numerator;
       aspect_ratio_denominator = aspect_denominator;
       chroma_format = Schroedinger.Chroma_420
    }
  in
  let enc = Schroedinger.Encoder.create video_format in
  Schroedinger.Encoder.set_settings enc 
    {  (Schroedinger.Encoder.get_settings enc) with 
          Schroedinger.Encoder.
           rate_control = Schroedinger.Encoder.Constant_noise_threshold;
           noise_threshold = quality;
    };
  let started = ref false in
  let header_encoder os = 
    Schroedinger.Encoder.encode_header enc os;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os = 
    let serialno = Ogg.Stream.serialno os in
    Some (Schroedinger.Skeleton.fisbone ~serialno ~format:video_format ())
  in
  let yuv = Image.YUV420.create width height in
  let (y,y_stride), (u, v, uv_stride) = Image.YUV420.internal yuv in
  let dirac_yuv =
  {
   Schroedinger.
    planes = [|(y,y_stride);(u,uv_stride);(v,uv_stride)|];
    frame_width = width;
    frame_height = height;
    format = Schroedinger.Yuv_420_p
  }
  in
  let convert =
    Video_converter.find_converter
      (P.RGB P.RGBA32)
      (P.YUV P.YUVJ420)
  in
  let stream_start os = [] in
  let data_encoder data os add_page = 
    let b,ofs,len = data.Ogg_muxer.data,data.Ogg_muxer.offset,
                    data.Ogg_muxer.length 
    in
    for i = ofs to ofs+len-1 do
      let frame = Img.of_RGBA32 b.(i) in
      convert
        frame 
        (Img.of_YUV420 yuv);
      Schroedinger.Encoder.encode_frame enc dirac_yuv os;
      if not !started then
       begin
        started := true;
        (* Try to add immediately the 
         * first page. *)
        try
          add_page (Ogg.Stream.flush_page os)
        with
          | Ogg.Not_enough_data -> ()
       end;
    done
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos = Int64.minus_one then
      Ogg_muxer.Unknown
    else
      Ogg_muxer.Time 
       (Int64.to_float 
        (Schroedinger.Encoder.encoded_of_granulepos 
           (Ogg.Page.granulepos p) enc) /.
           (float fps))
  in 
  let end_of_stream os =
    (* Encode at least some data.. *)
    if not !started then
     begin
      Image.YUV420.blank_all yuv;
      Schroedinger.Encoder.encode_frame enc dirac_yuv os
     end;
    Schroedinger.Encoder.eos enc os
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

let create_dirac =
  function
    | Encoder.Ogg.Dirac dirac ->
       let reset ogg_enc m =
         let metadata =
           Utils.list_of_metadata (Encoder.Meta.to_metadata m)
         in
         let enc =
           create_encoder ~metadata dirac
         in
         Ogg_muxer.register_track ?fill:dirac.Encoder.Dirac.fill ogg_enc enc
       in
       {
        Ogg_encoder.
           encode = Ogg_encoder.encode_video ;
           reset  = reset  ;
           id     = None
       }
    | _ -> assert false

let () = Hashtbl.add Ogg_encoder.encoders "dirac" create_dirac
