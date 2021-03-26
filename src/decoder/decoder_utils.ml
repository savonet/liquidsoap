(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

type samplerate_converter =
  samplerate:int -> Frame_content.Audio.data -> Frame_content.Audio.data

let samplerate_converter () =
  let state = ref None in
  let audio_dst_rate = float (Lazy.force Frame.audio_rate) in
  fun ~samplerate audio_buf ->
    let _channels = Array.length audio_buf in
    let ratio = audio_dst_rate /. float samplerate in
    match (!state, ratio) with
      (* All data is copied away from the decoders so that we can
       * pass it around uncopied in all future processing. *)
      | Some (_, channels), 1. when channels = _channels -> Audio.copy audio_buf
      | Some (converter, channels), _ when channels = _channels ->
          Audio_converter.Samplerate.resample converter ratio audio_buf
      | _ ->
          let converter = Audio_converter.Samplerate.create _channels in
          state := Some (converter, _channels);
          Audio_converter.Samplerate.resample converter ratio audio_buf

type wav_converter = string -> Frame_content.Audio.data

let from_iff ~format ~channels ~samplesize =
  let sample_bytes = samplesize / 8 in
  let buf = Buffer.create Utils.pagesize in
  fun src ->
    Buffer.add_string buf src;
    let src = Buffer.contents buf in
    let src_len = String.length src in
    let elem_len = sample_bytes * channels in
    let sample_len = src_len / elem_len in
    let sample_bytes_len = sample_len * elem_len in
    Buffer.reset buf;
    Buffer.add_substring buf src sample_bytes_len (src_len - sample_bytes_len);
    let dst = Audio.create channels sample_len in
    let to_audio =
      match samplesize with
        | 8 -> Audio.U8.to_audio
        | 16 when format = `Wav -> Audio.S16LE.to_audio
        | 16 when format = `Aiff -> Audio.S16BE.to_audio
        | 24 when format = `Wav -> Audio.S24LE.to_audio
        | 32 when format = `Wav -> Audio.S32LE.to_audio
        | _ -> failwith "unsuported sample size"
    in
    to_audio src 0 dst;
    dst

type channels_converter = Frame_content.Audio.data -> Frame_content.Audio.data

let channels_converter dst =
  let converter = ref None in
  fun data ->
    let _src = Array.length data in
    match !converter with
      | Some (c, src) when src = _src ->
          Audio_converter.Channel_layout.convert c data
      | _ ->
          let src = Audio_converter.Channel_layout.layout_of_channels _src in
          let c = Audio_converter.Channel_layout.create src dst in
          converter := Some (c, _src);
          Audio_converter.Channel_layout.convert c data

let video_scale () =
  let dst_width = Lazy.force Frame.video_width in
  let dst_height = Lazy.force Frame.video_height in
  let video_scale = Video_converter.scaler () ~proportional:true in
  fun img ->
    let src_width = Video.Image.width img in
    let src_height = Video.Image.height img in
    if (src_width, src_height) = (dst_width, dst_height) then img
    else (
      let img2 = Video.Image.create dst_width dst_height in
      video_scale img img2;
      img2)

type fps = { num : int; den : int }

(** Stupid nearest neighbour resampling.
  * For meaningful results, one should first partially apply the freq params,
  * and re-use the resulting functions on consecutive chunks of a single
  * input stream. *)
let video_resample ~in_freq ~out_freq =
  (* We have something like this:
   *
   * i i i i i i i i i i i i i i i i i i i ...
   * o     o       o     o       o     o   ...
   *
   * (1) We ensure that out_len/out_freq = in_len/in_freq asymptotically.
   *     For doing so, we must keep track of the full input length,
   *     modulo in_freq.
   * (2) We do the simplest possible thing to choose which i becomes
   *     which o: nearest neighbour in the currently available buffer.
   *     This is not as good as nearest neighbour in the real stream.
   *
   * Turns out the same code codes for when out_freq>in_freq too. *)
  let in_pos = ref 0 in
  let in_freq = in_freq.num * out_freq.den
  and out_freq = out_freq.num * in_freq.num in
  let ratio = out_freq / in_freq in
  fun input off len ->
    let new_in_pos = !in_pos + len in
    let already_out_len = !in_pos * ratio in
    let needed_out_len = new_in_pos * ratio in
    let out_len = needed_out_len - already_out_len in
    in_pos := new_in_pos mod in_freq;
    Array.init out_len (fun i -> input.(off + (i * ratio)))

let video_resample () =
  let state = ref None in
  let exec resampler data = resampler data 0 (Array.length data) in
  fun ~in_freq ~out_freq data ->
    if in_freq = out_freq then data
    else (
      match !state with
        | Some (resampler, _in_freq, _out_freq)
          when in_freq = _in_freq && out_freq = _out_freq ->
            exec resampler data
        | _ ->
            let resampler = video_resample ~in_freq ~out_freq in
            state := Some (resampler, in_freq, out_freq);
            exec resampler data)
