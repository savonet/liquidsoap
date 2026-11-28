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

open Mm

(** AVI encoder *)

open Avi_format

let log = Log.make ["avi"; "encoder"]

let encode_frame ~channels ~samplerate ~width ~height ~converter frame =
  let target_width = width in
  let target_height = height in
  let ratio = float samplerate /. float (Lazy.Mutexed.force Frame.audio_rate) in
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
    (* A frame of the stream with no image adds no image to the file. *)
    if vbuf.Content.Video.data = [] then Strings.empty
    else Avi.video_chunk_strings data
  in
  (video, Strings.of_string audio)

(* What the file form completes at the end: the sizes the streaming form
   leaves at their maximum, and an index of the chunks, which the writer wrote
   itself. *)
type index = {
  mutable data_length : int;
  mutable video_chunks : int;
  mutable audio_samples : int;
  entries : Buffer.t;
}

let dword n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Bytes.unsafe_to_string b

let add_entry index id chunk =
  let length = Strings.length chunk in
  Buffer.add_string index.entries id;
  Buffer.add_string index.entries (dword 0x10);
  (* Offsets count from the "movi" fourcc. *)
  Buffer.add_string index.entries (dword (4 + index.data_length));
  Buffer.add_string index.entries (dword (length - 8));
  index.data_length <- index.data_length + length

let complete_sizes (sink : Encoder.sink) ~header index seek =
  let header_length = String.length header in
  let idx1 = Buffer.contents index.entries in
  sink.write (Strings.of_list ["idx1"; dword (String.length idx1); idx1]);
  let total = header_length + index.data_length + 8 + String.length idx1 in
  let patch offset value =
    seek offset;
    sink.write (Strings.of_string (dword value))
  in
  (* The stream lengths of the two stream headers, the video's first. *)
  let rec strh_offsets from =
    if from + 4 > header_length then []
    else if String.sub header from 4 = "strh" then
      from :: strh_offsets (from + 4)
    else strh_offsets (from + 1)
  in
  patch 4 (total - 8);
  patch 44 0x0110;
  patch 48 index.video_chunks;
  (match strh_offsets 0 with
    | [video; audio] ->
        patch (video + 8 + 32) index.video_chunks;
        patch (audio + 8 + 32) index.audio_samples
    | _ -> ());
  patch (header_length - 8) (4 + index.data_length)

(* RIFF sizes are 32-bit: past 4 GiB, the file keeps the sizes of its streaming
   form and has no index. *)
let complete sink ~header index seek =
  let total =
    String.length header + index.data_length + 8 + Buffer.length index.entries
  in
  if total <= 0xffffffff then complete_sizes sink ~header index seek

let writer avi { Encoder.capabilities; sink; _ } =
  let channels = avi.channels in
  let samplerate = Deferred.force avi.samplerate in
  let converter = Audio_converter.Samplerate.create channels in
  let width = Deferred.force avi.width in
  let height = Deferred.force avi.height in
  log#info "Encoding at %dx%d, %d channels, %d Hz.%!" width height channels
    samplerate;
  let header = Avi.header ~width ~height ~channels ~samplerate () in
  let index =
    match sink.Encoder.seek with
      | Some seek when Encoder.file_form capabilities ->
          Some
            ( seek,
              {
                data_length = 0;
                video_chunks = 0;
                audio_samples = 0;
                entries = Buffer.create 4096;
              } )
      | _ -> None
  in
  let need_header = ref true in
  let encode frame =
    let video, audio =
      encode_frame ~channels ~samplerate ~width ~height ~converter frame
    in
    Option.iter
      (fun (_, index) ->
        if not (Strings.is_empty video) then (
          add_entry index "00db" video;
          index.video_chunks <- index.video_chunks + 1);
        add_entry index "01wb" audio;
        index.audio_samples <-
          index.audio_samples + ((Strings.length audio - 8) / (2 * channels)))
      index;
    let ans = Strings.append video audio in
    if !need_header then (
      need_header := false;
      Strings.dda header ans)
    else ans
  in
  let before_end () =
    Option.iter (fun (seek, index) -> complete sink ~header index seek) index
  in
  Encoder.writer_of_encoder ~before_end
    {
      Encoder.encode_metadata = (fun _ -> ());
      encode;
      header =
        (fun () ->
          if !need_header then Strings.empty else Strings.of_string header);
      (* A last audio chunk with what the rate converter still holds. *)
      stop =
        (fun () ->
          let tail = Audio_converter.Samplerate.drain converter in
          let len = Audio.length tail in
          if len = 0 then Strings.empty
          else (
            let data = Bytes.create (2 * channels * len) in
            Audio.S16LE.of_audio tail 0 data 0 len;
            let chunk =
              Strings.of_string (Avi.audio_chunk (Bytes.unsafe_to_string data))
            in
            Option.iter
              (fun (_, index) ->
                add_entry index "01wb" chunk;
                index.audio_samples <- index.audio_samples + len)
              index;
            chunk));
      bitrate =
        (fun () ->
          Some
            ((width * height * 12 * Lazy.Mutexed.force Frame.video_rate)
            + (16 * channels * samplerate)));
      (* Each call's output starts with a video chunk: a reader can start
         there. *)
      access_points = None;
      restart_offset = (fun () -> None);
    }
    sink

let () =
  Plug.register Encoder.plug "avi" ~doc:"Native avi encoder." (function
    | Encoder.AVI avi -> Some (writer avi)
    | _ -> None)
