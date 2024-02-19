(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

let load_image fname =
  let container = Av.open_input fname in
  let _, stream, codec = Av.find_best_video_stream container in
  let pixel_format =
    match Avcodec.Video.get_pixel_format codec with
      | None -> failwith "Pixel format unknown!"
      | Some f -> f
  in
  let width = Avcodec.Video.get_width codec in
  let height = Avcodec.Video.get_height codec in
  (* Hardcoding this instead of using Ffmpeg_utils.liq_frame_pixel_format () in
     order to have alpha channel. *)
  let out_pixel_format = `Yuva420p in
  let scaler =
    Scaler.create [] width height pixel_format width height out_pixel_format
  in
  match Av.read_input ~video_frame:[stream] container with
    | `Video_frame (_, frame) ->
        let frame = Scaler.convert scaler frame in
        Some (Ffmpeg_utils.unpack_image ~width ~height frame)
    | _ -> None

let () =
  Plug.register Decoder.image_file_decoders "ffmpeg"
    ~doc:"Decode images using Ffmpeg." (fun filename ->
      let ext = Filename.extension filename in
      if
        List.exists
          (fun s -> ext = "." ^ s)
          Ffmpeg_decoder.image_file_extensions#get
      then load_image filename
      else None)
