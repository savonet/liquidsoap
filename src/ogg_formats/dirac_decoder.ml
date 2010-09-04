(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

let check = Schroedinger.Decoder.check

let decoder os =
  let decoder = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
  let latest_yuv = ref None in
  let fill feed =
    let decoder,fps = 
      match !decoder with
        | Some (dec,fps) -> dec,fps
        | None ->
            let get_packet packet = 
              match !packet with
                | Some x -> x
                | None   -> 
                      let p = Ogg.Stream.get_packet os in
                      packet := Some p;
                      p
            in
            let packet1 = get_packet packet1 in
            let packet2 = get_packet packet2 in
            let dec = Schroedinger.Decoder.create packet1 packet2 in
            let video_format = Schroedinger.Decoder.get_video_format dec in
            let fps = (float video_format.Schroedinger.frame_rate_numerator) /.
                      (float video_format.Schroedinger.frame_rate_denominator)
            in
            decoder := Some (dec,fps);
            dec,fps
    in
    let ret = 
     try
      let yuv = Schroedinger.Decoder.decode_frame decoder os in
      latest_yuv := Some yuv ;
      yuv
     with
       | Schroedinger.Decoder.Skipped_frame -> 
          begin
            match !latest_yuv with
              | Some yuv -> yuv
              | None     -> assert false
          end
    in
    let ret =
      let format = 
        match ret.Schroedinger.format with
          | Schroedinger.Yuv_422_p -> Ogg_demuxer.Yuvj_422
          | Schroedinger.Yuv_444_p -> Ogg_demuxer.Yuvj_444
          | Schroedinger.Yuv_420_p -> Ogg_demuxer.Yuvj_420
    in
    {
      Ogg_demuxer.
        width   = ret.Schroedinger.frame_width;
        height  = ret.Schroedinger.frame_height;
        y_stride  = snd ret.Schroedinger.planes.(0);
        uv_stride = snd ret.Schroedinger.planes.(1);
        fps       = fps;
        format    = format;
        y = fst ret.Schroedinger.planes.(0);
        u = fst ret.Schroedinger.planes.(1);
        v = fst ret.Schroedinger.planes.(2)
    }
    in
    feed (ret,None)
  in
  Ogg_demuxer.Video fill

let () = Ogg_demuxer.ogg_decoders#register "dirac" (check,decoder)

