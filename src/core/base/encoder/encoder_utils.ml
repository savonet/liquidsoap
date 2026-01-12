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

(* See: https://datatracker.ietf.org/doc/html/rfc8216#section-3.4 *)
let render_mpeg2_timestamp =
  let mpeg2_timestamp_unit = 90000. in
  let frame_len =
    Lazy.from_fun (fun () ->
        Int64.of_float
          (Frame.seconds_of_main (Lazy.force Frame.size) *. mpeg2_timestamp_unit))
  in
  fun ~frame_position ~sample_position () ->
    let buf = Buffer.create 10 in
    let frame_position =
      Int64.mul (Lazy.force frame_len) (Int64.of_int frame_position)
    in
    let sample_position =
      Int64.of_float
        (Frame.seconds_of_main sample_position *. mpeg2_timestamp_unit)
    in
    let position = Int64.add frame_position sample_position in
    let position = Int64.unsigned_rem position 0x1ffffffffL in
    Buffer.add_int64_be buf position;
    Buffer.contents buf

let mk_hls_id3 ?(id3_version = 3) ~frame_position ~sample_position m =
  let timestamp =
    Printf.sprintf "com.apple.streaming.transportStreamTimestamp\000%s"
      (render_mpeg2_timestamp ~frame_position ~sample_position ())
  in
  let m = ("PRIV", timestamp) :: m in
  Utils.id3v2_of_metadata ~version:id3_version m

let mk_id3_hls ~pos encoder =
  let id3_version_ref = ref None in
  let init ?id3_enabled ?id3_version () =
    id3_version_ref := id3_version;
    if id3_enabled = Some false then
      Lang_encoder.raise_error ~pos "Format requires ID3 metadata!";
    true
  in
  let insert_id3 ~frame_position ~sample_position m =
    Some
      (mk_hls_id3 ?id3_version:!id3_version_ref ~frame_position ~sample_position
         m)
  in
  Encoder.{ (dummy_hls encoder) with init; insert_id3 }
