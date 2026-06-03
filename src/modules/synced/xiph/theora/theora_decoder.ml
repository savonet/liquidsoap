(*
 * Copyright 2007-2011 Savonet team
 *
 * This file is part of ocaml-theora.
 *
 * ocaml-theora is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-theora is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-theora; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let check = Theora.Decoder.check

let decoder ~fill:_ os =
  let decoder = Theora.Decoder.create () in
  let data = ref None in
  let latest_yuv = ref None in
  let os = ref os in
  let init () =
    match !data with
      | Some (dec, info, m) -> (dec, info, m)
      | None ->
          let packet = Ogg.Stream.get_packet !os in
          let decoder, info, vendor, m =
            Theora.Decoder.headerin decoder packet
          in
          let meta = (vendor, m) in
          data := Some (decoder, info, meta);
          (decoder, info, meta)
  in
  let decode feed =
    let decoder, info, _ = init () in
    let ret =
      try
        let yuv = Theora.Decoder.get_yuv decoder !os in
        latest_yuv := Some yuv;
        yuv
      with Theora.Duplicate_frame -> (
        match !latest_yuv with
          | Some yuv -> yuv
          | None -> raise Theora.Internal_error)
    in
    let format =
      match info.Theora.pixel_fmt with
        | Theora.PF_420 -> Ogg_decoder.Yuvj_420
        | Theora.PF_reserved -> assert false
        | Theora.PF_422 -> Ogg_decoder.Yuvj_422
        | Theora.PF_444 -> Ogg_decoder.Yuvj_444
    in
    let ret =
      {
        Ogg_decoder.format;
        frame_width = info.Theora.frame_width;
        frame_height = info.Theora.frame_height;
        y_stride = ret.Theora.y_stride;
        uv_stride = ret.Theora.u_stride;
        y = ret.Theora.y;
        u = ret.Theora.u;
        v = ret.Theora.v;
      }
    in
    feed ret
  in
  let info () =
    let _, info, m = init () in
    ( {
        Ogg_decoder.fps_numerator = info.Theora.fps_numerator;
        fps_denominator = info.Theora.fps_denominator;
        width = info.Theora.frame_width;
        height = info.Theora.frame_height;
      },
      m )
  in
  let restart ~fill:_ new_os = os := new_os in
  let samples_of_granulepos pos =
    let decoder, _, _ = init () in
    Theora.Decoder.frames_of_granulepos decoder pos
  in
  Ogg_decoder.Video
    {
      Ogg_decoder.name = "theora";
      info;
      decode;
      restart;
      samples_of_granulepos;
    }

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "theora" (check, decoder)
