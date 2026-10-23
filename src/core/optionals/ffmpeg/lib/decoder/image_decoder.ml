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

let with_input file fn =
  let container = Av.open_input file in
  Fun.protect ~finally:(fun () -> Av.close container) (fun () -> fn container)

let read_frame container =
  let _, stream, params = Av.find_best_video_stream container in
  let rec read () =
    match Av.read_input ~video_frame:[stream] container with
      | `Video_frame (_, frame) -> frame
      | _ -> read ()
  in
  (stream, params, read ())

(* image-decoder.md §2. *)
let check_image file =
  Decoder_settings.is_image_extension file
  &&
    try
      with_input file (fun container -> ignore (read_frame container));
      true
    with exn ->
      log#info "Cannot decode %s: %s"
        (Lang_string.quote_string file)
        (Printexc.to_string exn);
      false

let to_image frame =
  let format = Avutil.Video.frame_format frame in
  let scaler =
    Scaler.create
      ~threads:(max 0 Settings.scaling_threads#get)
      [Pixel_formats.scaling_flag ()]
      format.width format.height format.pixel_format format.width format.height
      `Yuva420p
  in
  Image_pack.unpack ~width:format.width ~height:format.height
    (Scaler.convert scaler frame)

(* §3; a failure raises, which the generic image decoding takes as no
   picture. *)
let decode_image file =
  try
    with_input file (fun container ->
        let stream, params, frame = read_frame container in
        let display =
          Display.create ~params ~time_base:(Av.get_time_base stream) ()
        in
        let kept = ref None in
        let keep f = if !kept = None then kept := Some f in
        Display.convert display keep frame;
        Display.eof display keep;
        match !kept with
          | Some frame -> to_image frame
          | None -> failwith "the display stage delivered no frame")
  with exn ->
    log#severe "Cannot decode image %s: %s"
      (Lang_string.quote_string file)
      (Printexc.to_string exn);
    raise exn

let () =
  Plug.register Decoder.image_file_decoders "ffmpeg"
    ~doc:"Decode images using FFmpeg."
    {
      Decoder.image_decoder_priority =
        (fun () -> Decoder_settings.image_priority#get);
      check_image;
      decode_image;
    }
