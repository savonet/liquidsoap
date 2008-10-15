(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
        uv_width  = ret.Theora.uv_width;
        uv_height = ret.Theora.uv_height;
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

