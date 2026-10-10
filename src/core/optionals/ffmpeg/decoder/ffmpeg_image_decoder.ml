(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

module Scaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

let log = Log.make ["decoder"; "ffmpeg"; "image"]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_image_priorities#plug "ffmpeg")
    "Priority for the ffmpeg image decoder" ~d:10

let check_container fname =
  let container = Av.open_input fname in
  try
    Fun.protect
      ~finally:(fun () -> Av.close container)
      (fun () ->
        let _, stream, _ = Av.find_best_video_stream container in
        let _ = Av.read_input ~video_frame:[stream] container in
        true)
  with exn ->
    log#info "Failed to decode %s: %s"
      (Lang_string.quote_string fname)
      (Printexc.to_string exn);
    false

let check_image filename =
  let ext = Filename.extension filename in
  List.exists
    (fun s -> ext = "." ^ s)
    Ffmpeg_decoder_conf.image_file_extensions#get
  && check_container filename

let decode_image fname =
  let container = Av.open_input fname in
  Fun.protect
    ~finally:(fun () -> Av.close container)
    (fun () ->
      let _, stream, codec = Av.find_best_video_stream container in
      let display =
        Ffmpeg_avfilter_utils.Display.init ~params:codec
          ~time_base:(Av.get_time_base stream) ()
      in
      let unpack frame =
        let width = Avutil.Video.frame_get_width frame in
        let height = Avutil.Video.frame_get_height frame in
        let scaler =
          Scaler.create
            ~threads:(Ffmpeg_utils.scaling_threads ())
            [] width height
            (Avutil.Video.frame_get_pixel_format frame)
            width height Ffmpeg_utils.liq_frame_pixel_format_with_alpha
        in
        Ffmpeg_utils.unpack_image ~width ~height (Scaler.convert scaler frame)
      in
      match Av.read_input ~video_frame:[stream] container with
        | `Video_frame (_, frame) -> (
            let image = ref None in
            let keep frame =
              if !image = None then image := Some (unpack frame)
            in
            Ffmpeg_avfilter_utils.Display.convert display frame keep;
            Ffmpeg_avfilter_utils.Display.eof display keep;
            match !image with Some image -> image | None -> raise Not_found)
        | _ -> raise Not_found)

let () =
  Plug.register Decoder.image_file_decoders "ffmpeg"
    ~doc:"Decode images using Ffmpeg."
    {
      Decoder.image_decoder_priority = (fun () -> priority#get);
      check_image;
      decode_image;
    }
