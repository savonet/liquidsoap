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

open Video_converter
open FFmpeg
module Img = Image.Generic
module P = Img.Pixel

let formats =
  [ P.RGB P.RGB24;
    P.RGB P.BGR24;
    P.RGB P.RGB32;
    P.RGB P.BGR32;
    P.RGB P.RGBA32;
    P.YUV P.YUV422;
    P.YUV P.YUV444;
    P.YUV P.YUV411;
    P.YUV P.YUV410;
    P.YUV P.YUVJ420;
    P.YUV P.YUVJ422;
    P.YUV P.YUVJ444 ]

let format_of frame =
  let fmt = Img.pixel_format frame in
  match fmt with
    | P.RGB fmt -> (
      match fmt with
        | P.RGB24 ->
            `Rgb24
        | P.BGR24 ->
            `Bgr24
        | P.RGB32 ->
            `Rgba
        | P.BGR32 ->
            `Bgra
        | P.RGBA32 ->
            `Rgba )
    | P.YUV fmt -> (
      match fmt with
        | P.YUV422 ->
            `Yuv422p
        | P.YUV444 ->
            `Yuv444p
        | P.YUV411 ->
            `Yuv411p
        | P.YUV410 ->
            `Yuv410p
        | P.YUVJ420 ->
            `Yuv420p
        | P.YUVJ422 ->
            `Yuvj422p
        | P.YUVJ444 ->
            `Yuvj444p )

type fmt = Avutil.Pixel_format.t * int * int

type conv = {conv: Swscale.t; dst_off: [`Pixel of int | `Line of int | `Zero]}

(* TODO: share this with Gavl. *)
module HT = struct
  type t = (bool * fmt * fmt) * conv option

  let equal (fmt, _) (fmt', _) = fmt = fmt'

  let hash (fmt, _) = Hashtbl.hash fmt
end

module WH = struct
  include Weak.Make (HT)

  (* Number of converters to always keep in memory. *)
  let n = 2

  let keep = Array.make n None

  let add h fmt conv =
    let conv = (fmt, Some conv) in
    for i = 1 to n - 1 do
      keep.(i - 1) <- keep.(i)
    done ;
    keep.(n - 1) <- Some conv ;
    add h conv

  let assoc h fmt = Utils.get_some (snd (find h (fmt, None)))
end

(* Weak hashtable containing converters already created. *)
let converters = WH.create 5

let is_rgb = function P.RGB _ -> true | _ -> false

let create () =
  let convert ~proportional src dst =
    let src_f = format_of src in
    let dst_f = format_of dst in
    let src_w = Img.width src in
    let src_h = Img.height src in
    let dst_w = Img.width dst in
    let dst_h = Img.height dst in
    let conv =
      try
        WH.assoc converters
          (proportional, (src_f, src_w, src_h), (dst_f, dst_w, dst_h))
      with Not_found ->
        let dst_off, dst_w, dst_h =
          if proportional then
            if dst_h * src_w < src_h * dst_w then (
              let ox = (dst_w - (src_w * dst_h / src_h)) / 2 in
              (`Pixel ox, dst_w - (2 * ox), dst_h) )
            else (
              let oy = (dst_h - (src_h * dst_w / src_w)) / 2 in
              (`Line oy, dst_w, dst_h - (2 * oy)) )
          else (`Zero, dst_w, dst_h)
        in
        let conv =
          Swscale.create
            [Swscale.Bilinear; Swscale.Print_info]
            src_w src_h src_f dst_w dst_h dst_f
        in
        let conv = {conv; dst_off} in
        WH.add converters
          (proportional, (src_f, src_w, src_h), (dst_f, dst_w, dst_h))
          conv ;
        conv
    in
    let data f =
      match Img.pixel_format f with
        | P.RGB _ ->
            let buf, stride = Img.rgb_data f in
            if conv.dst_off <> `Zero then Bigarray.Array1.fill buf 0 ;
            [|(buf, stride)|]
        | P.YUV _ ->
            let (y, sy), (u, v, s) = Img.yuv_data f in
            [|(y, sy); (u, s); (v, s)|]
    in
    let src_d = data src in
    let dst_d = data dst in
    let dst_off =
      (* Since swscale does not know how to scale keeping aspect ratio, we have
         to play a bit with offsets... *)
      match conv.dst_off with
        | `Zero ->
            0
        | `Line n ->
            assert (n = 0 || Array.length dst_d = 1) ;
            n * snd dst_d.(0)
        | `Pixel n ->
            assert (n = 0 || Array.length dst_d = 1) ;
            n * Avutil.Pixel_format.bits dst_f
    in
    Swscale.scale conv.conv src_d 0 src_h dst_d dst_off
  in
  convert

let () = video_converters#register "ffmpeg" (formats, formats, create)
