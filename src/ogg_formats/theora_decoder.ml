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
  let decoder = Theora.Decoder.create () in
  let data    = ref None in
  let latest_yuv = ref None in
  let fill feed =
     let m,fps = 
       match !data with 
         | Some fps -> None,fps
         | None -> 
            begin
             let packet = Ogg.Stream.get_packet os in
             try
              let (info,vendor,m) = 
                Theora.Decoder.headerin decoder packet 
              in
              let fps = (float (info.Theora.fps_numerator)) /.
                        (float (info.Theora.fps_denominator))
              in
              data := Some fps ;
              Some (vendor,m),fps
             with
               | Theora.Not_enough_data -> raise Ogg.Not_enough_data
           end;
    in
    let ret = 
     try
      let yuv = Theora.Decoder.get_yuv decoder os in
      latest_yuv := Some yuv ;
      yuv
     with
       | Theora.Duplicate_frame -> 
          begin
            match !latest_yuv with
              | Some yuv -> yuv
              | None     -> raise Theora.Internal_error
          end
    in
    let ret =
    {
      Ogg_demuxer.
        y_width   = ret.Theora.y_width;
        y_height  = ret.Theora.y_height;
        y_stride  = ret.Theora.y_stride;
        (** TODO: make sure this is actually correct.. *)
        uv_width  = ret.Theora.u_width;
        uv_height = ret.Theora.u_height;
        uv_stride = ret.Theora.u_stride;
        fps       = fps;
        y = ret.Theora.y;
        u = ret.Theora.u;
        v = ret.Theora.v
    }
    in
    feed (ret,m)
  in
  Ogg_demuxer.Video fill

let () = Ogg_demuxer.ogg_decoders#register "theora" (check,decoder)

