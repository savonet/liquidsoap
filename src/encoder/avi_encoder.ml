(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

(** AVI encoder *)

open Encoder.AVI

module Img = Image.RGBA32

let encode_frame ~channels ~samplerate ~converter frame start len =
  let ratio = float samplerate /. float (Lazy.force Frame.audio_rate) in
  let content =
    Frame.content_of_type frame start
      { Frame.
        audio = channels;
        video = 1;
        midi = 0;
      }
  in
  let audio =
    let astart = Frame.audio_of_master start in
    let alen = Frame.audio_of_master len in
    let pcm = content.Frame.audio in
    (* Resample if needed. *)
    let pcm,astart,alen =
      if ratio = 1. then
        pcm, astart, alen
      else
        let pcm = Audio_converter.Samplerate.resample converter ratio pcm astart alen in
        pcm, 0, Array.length pcm.(0)
    in
    let data = Bytes.create (2*channels*alen) in
    Audio.S16LE.of_audio pcm astart data 0 alen;
    Avi.audio_chunk data
  in
  let video =
    let vbuf = content.Frame.video in
    let vbuf = vbuf.(0) in
    let vstart = Frame.video_of_master start in
    let vlen = Frame.video_of_master len in
    let data = ref "" in
    for i = vstart to vstart+vlen-1 do
      (* TODO: mplayer needs flipping, but not vlc or GStreamer... *)
      (* Img.Effect.flip vbuf.(i); *)
      Img.swap_rb vbuf.(i);
      data := !data ^ Img.to_RGB24_string vbuf.(i)
    done;
    Avi.video_chunk !data
  in
  video ^ audio

let encoder avi =
  let channels = avi.channels in
  let samplerate = avi.samplerate in
  let converter = Audio_converter.Samplerate.create channels in
  (* TODO: use duration *)
  let header = Avi.header ~channels ~samplerate () in
  let need_header = ref true in
  let encode frame start len =
    let ans = encode_frame ~channels ~samplerate ~converter frame start len in
    if !need_header then
      (
        need_header := false;
        header ^ ans
      )
    else
      ans
  in
  {
    Encoder.
    insert_metadata = (fun _ -> ());
    encode = encode;
    header = Some header;
    stop = (fun () -> "")
  }

let () =
  Encoder.plug#register "AVI"
    (function
       | Encoder.AVI avi -> Some (fun _ _ -> encoder avi)
       | _ -> None)
