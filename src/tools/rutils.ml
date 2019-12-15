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

(** Resampling module *)

(** TODO: video *)

type audio_converter =
  ?audio_src_rate:float -> Frame.audio_t array -> Frame.audio_t array

let create_audio () =
  let audio_converters = Hashtbl.create 2 in
  let audio_dst_rate = float (Lazy.force Frame.audio_rate) in
  fun ?audio_src_rate audio_buf ->
    let process_audio audio_src_rate =
      (* Create new converters if needed, remove unused converters *)
      let new_audio_chans = Array.length audio_buf in
      let old_audio_chans = Hashtbl.length audio_converters in
      if old_audio_chans < new_audio_chans then
        for i = old_audio_chans to new_audio_chans - 1 do
          Hashtbl.add audio_converters i (Audio_converter.Samplerate.create 1)
        done;
      if new_audio_chans < old_audio_chans then
        for i = new_audio_chans to new_audio_chans - 1 do
          Hashtbl.remove audio_converters i
        done;
      let ratio = audio_dst_rate /. audio_src_rate in
      let resample_chan n buf =
        (* TODO: it's a bit weired to have to encapsulate as 1-channel... *)
        let resampler = Hashtbl.find audio_converters n in
        let ret =
          Audio_converter.Samplerate.resample resampler ratio [| buf |]
        in
        ret.(0)
      in
      Array.mapi resample_chan audio_buf
    in
    let audio_rate =
      match audio_src_rate with Some rate -> rate | None -> audio_dst_rate
    in
    process_audio audio_rate

type wav_converter = audio_src_rate:float -> string -> Frame.audio_t array

let create_from_iff ~format ~channels ~samplesize =
  let audio_dst_rate = float (Lazy.force Frame.audio_rate) in
  let sample_bytes = samplesize / 8 in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let buf = Buffer.create Utils.pagesize in
  fun ~audio_src_rate src ->
    let ratio = audio_dst_rate /. audio_src_rate in
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
    Audio_converter.Samplerate.resample samplerate_converter ratio dst
