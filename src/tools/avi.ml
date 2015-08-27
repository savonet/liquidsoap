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

let word n =
  let s = Bytes.create 2 in
  Bytes.set s 0 (char_of_int (n land 0xff));
  Bytes.set s 1 (char_of_int ((n land 0xff00) lsr 8));
  s

let dword n =
  let s = Bytes.create 4 in
  Bytes.set s 0 (char_of_int (n land 0xff));
  Bytes.set s 1 (char_of_int ((n land 0xff00) lsr 8));
  Bytes.set s 2 (char_of_int ((n land 0xff0000) lsr 16));
  Bytes.set s 3 (char_of_int ((n land 0x7f000000) lsr 24));
  s

let chunk id data =
  let n = Bytes.length data in
  let ans = id ^ dword n ^ data in
  if n mod 2 = 0 then
    ans
  else
    ans ^ "\000"

let list = chunk "LIST"

let header ~channels ~samplerate () =
  let max_dword = 0xffffffff in
  let file_size = max_dword in
  let video_rate = Lazy.force Frame.video_rate in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let avi_header =
    chunk
      "avih"
      (
        dword (1000000 / video_rate) (* microsec per frame *)
        ^ dword 0 (* maximum bytes per second *)
        ^ dword 0 (* reserved *)
        ^ dword 0x0100 (* flags (interleaved) *)
        ^ dword max_dword (* number of frames *)
        ^ dword 0 (* initial frame *)
        ^ dword 2 (* number of streams *)
        ^ dword 0 (* suggested buffer size *)
        ^ dword width (* width *)
        ^ dword height (* height *)
        ^ dword 0 (* reserved *)
        ^ dword 0 (* reserved *)
        ^ dword 0 (* reserved *)
        ^ dword 0 (* reserved *)
      )
  in
  let video_header =
    let stream_header =
      chunk
        "strh"
        (
          "vids" (* stream type *)
          ^ dword 0 (* stream codec *)
          ^ dword 0 (* flags *)
          ^ word 0 (* priority *)
          ^ word 0 (* language *)
          ^ dword 0 (* initial frames *)
          ^ dword 1 (* scale *)
          ^ dword video_rate (* rate *)
          ^ dword 0 (* start time *)
          ^ dword max_dword (* stream length *)
          ^ dword 0 (* suggested buffer size *)
          ^ dword 0xffffffff (* quality *)
          ^ dword 0 (* sample size *)
          ^ word 0 (* left *)
          ^ word 0 (* top *)
          ^ word width (* right *)
          ^ word height (* bottom *)
        )
    in
    let stream_format =
      (* see BITMAPINFO *)
      chunk
        "strf"
        (
          dword 40 (* size of this structure *)
          ^ dword width (* width *)
          ^ dword height (* height *)
          ^ word 1 (* panes *)
          ^ word 24 (* depth *)
          ^ dword 0 (* RGB uncompressed format *)
          ^ dword 0 (* image size *)
          ^ dword 0 (* pixels / x meter *)
          ^ dword 0 (* pixels / y meter *)
          ^ dword 0 (* colors used *)
          ^ dword 0 (* important colors *)
        )
    in
    list ("strl" ^ stream_header ^ stream_format)
  in
  let audio_header =
    let stream_header =
      chunk
        "strh"
        (
          "auds" (* stream type *)
          ^ dword 0 (* stream *)
          ^ dword 0 (* flags *)
          ^ word 0 (* priority *)
          ^ word 0 (* language *)
          ^ dword 0 (* initial frames *)
          ^ dword 1 (* scale *)
          ^ dword samplerate (* rate *)
          ^ dword 0 (* start time *)
          ^ dword max_dword (* stream length *)
          ^ dword 0 (* suggested buffer size *)
          ^ dword 0xffffffff (* quality *)
          ^ dword (2 * channels) (* sample size *)
          ^ word 0 (* left *)
          ^ word 0 (* top *)
          ^ word 0 (* right *)
          ^ word 0 (* bottom *)
        )
    in
    let stream_format =
      chunk
        "strf"
        (
          word 1 (* stream type (PCM) *)
          ^ word channels (* channels *)
          ^ dword samplerate (* rate *)
          ^ dword (2 * channels * samplerate) (* byte rate *)
          ^ word (2 * channels) (* block align *)
          ^ word 16 (* bits per sample *)
          ^ word 0 (* size of extra information *)
        )
    in
    list ("strl" ^ stream_header ^ stream_format)
  in
  let headers =
    list ("hdrl" ^ avi_header ^ video_header ^ audio_header)
  in
  let info =
    let producer = chunk "ISFT" Configure.vendor in
    list ("INFO" ^ producer)
  in
  "RIFF"
  ^ dword file_size
  ^ "AVI "
  ^ headers
  ^ info
  ^ "LIST" ^ dword max_dword ^ "movi"

(* Audio in 16LE *)
let audio_chunk b =
  chunk "01wb" b

(* Video in RGB. *)
let video_chunk b =
  chunk "00db" b
