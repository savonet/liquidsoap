(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

let create_encoder ~theora ~metadata () =
   let quality,bitrate =
     match theora.Encoder.Theora.bitrate_control with
       | Encoder.Theora.Bitrate x -> 0,x
       | Encoder.Theora.Quality x -> x,0
  in
  let width              = Lazy.force theora.Encoder.Theora.width in
  let height             = Lazy.force theora.Encoder.Theora.height in
  let picture_width      = Lazy.force theora.Encoder.Theora.picture_width in
  let picture_height     = Lazy.force theora.Encoder.Theora.picture_height in
  let picture_x          = theora.Encoder.Theora.picture_x in
  let picture_y          = theora.Encoder.Theora.picture_y in
  let aspect_numerator   = theora.Encoder.Theora.aspect_numerator in
  let aspect_denominator = theora.Encoder.Theora.aspect_denominator in
  let fps = Frame.video_of_seconds 1. in
  let version_major,version_minor,version_subminor = Theora.version_number in
  let keyframe_frequency = Some theora.Encoder.Theora.keyframe_frequency in
  let vp3_compatible     = theora.Encoder.Theora.vp3_compatible in
  let soft_target        = Some theora.Encoder.Theora.soft_target in
  let buffer_delay       = theora.Encoder.Theora.buffer_delay in
  let speed              = theora.Encoder.Theora.speed in
  let info =
    {
     Theora.
      frame_width = width;
      frame_height = height;
      picture_width = picture_width;
      picture_height = picture_height;
      picture_x = picture_x;
      picture_y = picture_y;
      fps_numerator = fps;
      fps_denominator = 1;
      aspect_numerator = aspect_numerator;
      aspect_denominator = aspect_denominator;
      colorspace = Theora.CS_unspecified;
      keyframe_granule_shift = Theora.default_granule_shift;
      target_bitrate = bitrate;
      quality = quality;
      version_major = version_major;
      version_minor = version_minor;
      version_subminor = version_subminor;
      pixel_fmt = Theora.PF_420
    }
  in
  let params = 
    {
     Theora.Encoder.
      keyframe_frequency = keyframe_frequency ;
      vp3_compatible     = vp3_compatible ;
      soft_target        = soft_target ;
      buffer_delay       = buffer_delay ;
      speed              = speed
    }
  in
  let enc =
    if width mod 16 <> 0 || height mod 16 <> 0 then
      failwith "Invalide theora width/height (should be a multiple of 16).";
    Theora.Encoder.create info params metadata
  in
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
    Ogg_muxer.flush_pages os
  in
  let yuv = Image.YUV420.create width height in
  let (y,y_stride), (u, v, uv_stride) = Image.YUV420.internal yuv in
  let theora_yuv =
  {
    Theora.y_width = width ;
    Theora.y_height = height ;
    Theora.y_stride = y_stride;
    Theora.u_width = width / 2;
    Theora.u_height = height / 2;
    Theora.u_stride = uv_stride;
    Theora.v_width = width / 2;
    Theora.v_height = height / 2;
    Theora.v_stride = uv_stride;
    Theora.y = y;
    Theora.u = u;
    Theora.v = v;
  }
  in
  let convert =
    Video_converter.find_converter
      (P.RGB P.RGBA32)
      (P.YUV P.YUVJ420)
  in
  let data_encoder data os _ = 
    if not !started then
      started := true;
    let b,ofs,len = data.Ogg_muxer.data,data.Ogg_muxer.offset,
                    data.Ogg_muxer.length 
    in
    for i = ofs to ofs+len-1 do
      let frame = Img.of_RGBA32 b.(i) in
      convert
        frame
        (Img.of_YUV420 yuv);
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
      Image.YUV420.blank_all yuv;
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
       let reset ogg_enc m =
         let metadata = 
           Utils.list_of_metadata (Encoder.Meta.to_metadata m) 
         in 
         let enc =
           create_encoder ~theora ~metadata ()
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
