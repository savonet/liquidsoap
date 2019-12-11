(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

module Img = Image.Generic

let create_encoder ~theora ~metadata () =
  let quality, bitrate =
    match theora.Theora_format.bitrate_control with
      | Theora_format.Bitrate x ->
          (0, x)
      | Theora_format.Quality x ->
          (x, 0)
  in
  let width = Lazy.force theora.Theora_format.width in
  let height = Lazy.force theora.Theora_format.height in
  let picture_width = Lazy.force theora.Theora_format.picture_width in
  let picture_height = Lazy.force theora.Theora_format.picture_height in
  let picture_x = theora.Theora_format.picture_x in
  let picture_y = theora.Theora_format.picture_y in
  let aspect_numerator = theora.Theora_format.aspect_numerator in
  let aspect_denominator = theora.Theora_format.aspect_denominator in
  let fps = Frame.video_of_seconds 1. in
  let version_major, version_minor, version_subminor = Theora.version_number in
  let keyframe_frequency = Some theora.Theora_format.keyframe_frequency in
  let vp3_compatible = theora.Theora_format.vp3_compatible in
  let soft_target = Some theora.Theora_format.soft_target in
  let buffer_delay = theora.Theora_format.buffer_delay in
  let speed = theora.Theora_format.speed in
  let info =
    {
      Theora.frame_width= width;
      frame_height= height;
      picture_width;
      picture_height;
      picture_x;
      picture_y;
      fps_numerator= fps;
      fps_denominator= 1;
      aspect_numerator;
      aspect_denominator;
      colorspace= Theora.CS_unspecified;
      keyframe_granule_shift= Theora.default_granule_shift;
      target_bitrate= bitrate;
      quality;
      version_major;
      version_minor;
      version_subminor;
      pixel_fmt= Theora.PF_420;
    }
  in
  let params =
    {
      Theora.Encoder.keyframe_frequency;
      vp3_compatible;
      soft_target;
      buffer_delay;
      speed;
    }
  in
  let enc =
    if width mod 16 <> 0 || height mod 16 <> 0 then
      failwith "Invalide theora width/height (should be a multiple of 16)." ;
    Theora.Encoder.create info params metadata
  in
  let started = ref false in
  let header_encoder os =
    Theora.Encoder.encode_header enc os ;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os =
    let serialno = Ogg.Stream.serialno os in
    Some (Theora.Skeleton.fisbone ~serialno ~info ())
  in
  let stream_start os = Ogg_muxer.flush_pages os in
  let yuv = Image.YUV420.create width height in
  let data_encoder data os _ =
    if not !started then started := true ;
    let b, ofs, len =
      (data.Ogg_muxer.data, data.Ogg_muxer.offset, data.Ogg_muxer.length)
    in
    for i = ofs to ofs + len - 1 do
      let img = Video.get b i in
      let theora_yuv =
        {
          Theora.y_width= width;
          Theora.y_height= height;
          Theora.y_stride= Image.YUV420.y_stride img;
          Theora.u_width= width / 2;
          Theora.u_height= height / 2;
          Theora.u_stride= Image.YUV420.uv_stride img;
          Theora.v_width= width / 2;
          Theora.v_height= height / 2;
          Theora.v_stride= Image.YUV420.uv_stride img;
          Theora.y= Image.YUV420.y img;
          Theora.u= Image.YUV420.u img;
          Theora.v= Image.YUV420.v img;
        }
      in
      Theora.Encoder.encode_buffer enc os theora_yuv
    done
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then Ogg_muxer.Unknown
    else if granulepos <> Int64.zero then (
      let index =
        Int64.succ (Theora.Encoder.frames_of_granulepos enc granulepos)
      in
      Ogg_muxer.Time (Int64.to_float index /. float fps) )
    else Ogg_muxer.Time 0.
  in
  let end_of_stream os =
    (* Encode at least some data.. *)
    if not !started then (
      let theora_yuv =
        {
          Theora.y_width= width;
          Theora.y_height= height;
          Theora.y_stride= width;
          Theora.u_width= width / 2;
          Theora.u_height= height / 2;
          Theora.u_stride= width / 2;
          Theora.v_width= width / 2;
          Theora.v_height= height / 2;
          Theora.v_stride= width / 2;
          Theora.y= Image.Data.alloc width;
          Theora.u= Image.Data.alloc (width / 2);
          Theora.v= Image.Data.alloc (width / 2);
        }
      in
      Image.YUV420.blank_all yuv ;
      Theora.Encoder.encode_buffer enc os theora_yuv ) ;
    Theora.Encoder.eos enc os
  in
  {
    Ogg_muxer.header_encoder;
    fisbone_packet;
    stream_start;
    data_encoder= Ogg_muxer.Video_encoder data_encoder;
    end_of_page;
    end_of_stream;
  }

let create_theora = function
  | Ogg_format.Theora theora ->
      let reset ogg_enc m =
        let metadata = Utils.list_of_metadata (Meta_format.to_metadata m) in
        let enc = create_encoder ~theora ~metadata () in
        Ogg_muxer.register_track ?fill:theora.Theora_format.fill ogg_enc enc
      in
      {Ogg_encoder.encode= Ogg_encoder.encode_video; reset; id= None}
  | _ ->
      assert false

let () = Hashtbl.add Ogg_encoder.encoders "theora" create_theora
