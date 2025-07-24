(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

open Mm

(** AVI encoder *)

open Avi_format

let log = Log.make ["avi"; "encoder"]

let encode_frame ~channels ~samplerate ~width ~height ~converter frame =
  let target_width = width in
  let target_height = height in
  let ratio = float samplerate /. float (Lazy.force Frame.audio_rate) in
  let audio =
    let alen = AFrame.position frame in
    let pcm = AFrame.pcm frame in
    let pcm, astart, alen =
      Audio_converter.Samplerate.resample converter ratio pcm 0 alen
    in
    let data = Bytes.create (2 * channels * alen) in
    Audio.S16LE.of_audio pcm astart data 0 alen;
    Avi.audio_chunk (Bytes.unsafe_to_string data)
  in
  let video =
    let vbuf = VFrame.data frame in
    let data = Strings.Mutable.empty () in
    let scaler = Video_converter.scaler () in
    List.iter
      (fun (_, img) ->
        let img =
          img
          |> Video.Canvas.Image.resize ~scaler ~proportional:true target_width
               target_height
          |> Video.Canvas.Image.render ~transparent:false
        in
        let width = Image.YUV420.width img in
        let height = Image.YUV420.height img in
        if width <> target_width || height <> target_height then
          failwith
            (Printf.sprintf
               "Resizing is not yet supported by AVI encoder got %dx%d instead \
                of %dx%d"
               width height target_width target_height);
        let y, u, v = Image.YUV420.data img in
        let y = Image.Data.to_string y in
        let u = Image.Data.to_string u in
        let v = Image.Data.to_string v in
        let y_stride = Image.YUV420.y_stride img in
        let uv_stride = Image.YUV420.uv_stride img in
        if y_stride = width then Strings.Mutable.add data y
        else
          for j = 0 to height - 1 do
            Strings.Mutable.add_substring data y (j * y_stride) width
          done;
        if uv_stride = width / 2 then (
          Strings.Mutable.add data u;
          Strings.Mutable.add data v)
        else (
          for j = 0 to (height / 2) - 1 do
            Strings.Mutable.add_substring data u (j * uv_stride) (width / 2)
          done;
          for j = 0 to (height / 2) - 1 do
            Strings.Mutable.add_substring data v (j * uv_stride) (width / 2)
          done))
      vbuf.Content.Video.data;
    Avi.video_chunk_strings data
  in
  Strings.add video audio

let encoder avi =
  let channels = avi.channels in
  let samplerate = Lazy.force avi.samplerate in
  let converter = Audio_converter.Samplerate.create channels in
  let width = Lazy.force avi.width in
  let height = Lazy.force avi.height in
  log#info "Encoding at %dx%d, %d channels, %d Hz.%!" width height channels
    samplerate;
  (* TODO: use duration *)
  let header = Avi.header ~width ~height ~channels ~samplerate () in
  let need_header = ref true in
  let encode frame =
    let ans =
      encode_frame ~channels ~samplerate ~width ~height ~converter frame
    in
    if !need_header then (
      need_header := false;
      Strings.dda header ans)
    else ans
  in
  {
    Encoder.encode_metadata = (fun _ -> ());
    hls = Encoder.dummy_hls encode;
    encode;
    header = (fun () -> Strings.of_string header);
    stop = (fun () -> Strings.empty);
  }

let () =
  Plug.register Encoder.plug "avi" ~doc:"Native avi encoder." (function
    | Encoder.AVI avi -> Some (fun ?hls:_ ~pos:_ _ _ -> encoder avi)
    | _ -> None)
