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

(** AVI encoder *)

open Avi_format

let encode_frame ~channels ~samplerate ~converter frame start len =
  let ratio = float samplerate /. float (Lazy.force Frame.audio_rate) in
  let content =
    Frame.content_of_type frame start
      {Frame.audio= channels; video= 1; midi= 0}
  in
  let audio =
    let astart = Frame.audio_of_master start in
    let alen = Frame.audio_of_master len in
    let pcm = content.Frame.audio in
    (* Resample if needed. *)
    let pcm, astart, alen =
      if ratio = 1. then (pcm, astart, alen)
      else (
        let pcm =
          Audio_converter.Samplerate.resample converter ratio
            (Audio.sub pcm astart alen)
        in
        (pcm, 0, Audio.length pcm) )
    in
    let data = Bytes.create (2 * channels * alen) in
    Audio.S16LE.of_audio (Audio.sub pcm astart alen) data 0 ;
    Avi.audio_chunk (Bytes.unsafe_to_string data)
  in
  let video =
    let vbuf = content.Frame.video in
    let vbuf = vbuf.(0) in
    let vstart = Frame.video_of_master start in
    let vlen = Frame.video_of_master len in
    let data = Strings.Mutable.empty () in
    for i = vstart to vstart + vlen - 1 do
      let img = Video.get vbuf i in
      (* TODO: change stride otherwise *)
      let width = Image.YUV420.width img in
      assert (Image.YUV420.y_stride img = width) ;
      assert (Image.YUV420.uv_stride img = width / 2) ;
      let y, u, v = Image.YUV420.data img in
      Strings.Mutable.add data (Image.Data.to_string y) ;
      Strings.Mutable.add data (Image.Data.to_string u) ;
      Strings.Mutable.add data (Image.Data.to_string v)
    done ;
    Avi.video_chunk_strings data
  in
  Strings.add video audio

let encoder avi =
  let channels = avi.channels in
  let samplerate = Lazy.force avi.samplerate in
  let converter = Audio_converter.Samplerate.create channels in
  (* TODO: use duration *)
  let header = Avi.header ~channels ~samplerate () in
  let need_header = ref true in
  let encode frame start len =
    let ans = encode_frame ~channels ~samplerate ~converter frame start len in
    if !need_header then (
      need_header := false ;
      Strings.dda header ans )
    else ans
  in
  {
    Encoder.insert_metadata= (fun _ -> ());
    encode;
    header= Strings.of_string header;
    stop= (fun () -> Strings.empty);
  }

let () =
  Encoder.plug#register "AVI" (function
    | Encoder.AVI avi ->
        Some (fun _ _ -> encoder avi)
    | _ ->
        None)
