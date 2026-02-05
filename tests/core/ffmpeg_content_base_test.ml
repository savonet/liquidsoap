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

(* Tests for Ffmpeg_content_base operations *)

let time_base = { Avutil.num = 1; den = 1000 }
let copy x = x

(* Helper to create data with packets at specific positions *)
let mk_data ?(stream_idx = 1L) ?(time_base = time_base) ~length data =
  { Ffmpeg_content_base.length; stream_idx; time_base; data }

(* Helper to create content with chunks *)
let mk_content params chunks : (unit option, int) Ffmpeg_content_base.content =
  { Ffmpeg_content_base.params; chunks }

(* Test basic content creation *)
let () =
  let content = Ffmpeg_content_base.make None in
  assert (Ffmpeg_content_base.length content = 0);
  assert (Ffmpeg_content_base.params content = None);
  assert (content.chunks = [])

(* Test content length calculation with multiple chunks *)
let () =
  let chunk1 = mk_data ~length:100 [(0, 1); (50, 2)] in
  let chunk2 = mk_data ~length:200 [(0, 3); (100, 4)] in
  let content = mk_content (Some ()) [chunk1; chunk2] in
  assert (Ffmpeg_content_base.length content = 300)

(* Test content with empty data list but positive length (sparse streams) *)
let () =
  let chunk = mk_data ~length:1000 [] in
  let content = mk_content (Some ()) [chunk] in
  assert (Ffmpeg_content_base.length content = 1000);
  assert (List.length (List.hd content.chunks).data = 0)

(* Test copy preserves structure *)
let () =
  let chunk = mk_data ~length:100 [(0, 1); (50, 2)] in
  let content = mk_content (Some ()) [chunk] in
  let copied = Ffmpeg_content_base.copy ~copy content in
  assert (Ffmpeg_content_base.length copied = 100);
  assert (List.length copied.chunks = 1);
  let copied_chunk = List.hd copied.chunks in
  assert (copied_chunk.length = 100);
  assert (List.length copied_chunk.data = 2)

(* Test sub_data filters by position *)
let () =
  let chunk = mk_data ~length:100 [(10, 1); (30, 2); (50, 3); (70, 4)] in
  (* Sub from 20 to 60 (length 40) should include positions 30 and 50 *)
  let sub = Ffmpeg_content_base.sub_data ~copy chunk 20 40 in
  assert (sub.length = 40);
  assert (sub.stream_idx = chunk.stream_idx);
  assert (sub.time_base = chunk.time_base);
  (* Positions should be adjusted: 30->10, 50->30 *)
  assert (List.length sub.data = 2);
  assert (List.assoc 10 sub.data = 2);
  assert (List.assoc 30 sub.data = 3)

(* Test sub_data with empty result *)
let () =
  let chunk = mk_data ~length:100 [(10, 1); (30, 2)] in
  (* Sub from 50 to 100 should have no data *)
  let sub = Ffmpeg_content_base.sub_data ~copy chunk 50 50 in
  assert (sub.length = 50);
  assert (List.length sub.data = 0)

(* Test blit: basic copy *)
let () =
  let src_chunk = mk_data ~length:100 [(0, 1); (50, 2)] in
  let src = mk_content (Some ()) [src_chunk] in
  let dst = mk_content None [] in
  Ffmpeg_content_base.blit ~copy src 0 dst 0 100;
  assert (Ffmpeg_content_base.length dst = 100);
  assert (Ffmpeg_content_base.params dst = Some ())

(* Test blit: partial source copy *)
let () =
  let src_chunk = mk_data ~length:100 [(10, 1); (30, 2); (50, 3); (70, 4)] in
  let src = mk_content (Some ()) [src_chunk] in
  let dst = mk_content None [] in
  (* Copy from position 20 to 60 (40 samples) *)
  Ffmpeg_content_base.blit ~copy src 20 dst 0 40;
  assert (Ffmpeg_content_base.length dst = 40);
  assert (List.length dst.chunks = 1);
  let dst_chunk = List.hd dst.chunks in
  (* Should have positions 30 and 50, adjusted to 10 and 30 *)
  assert (List.length dst_chunk.data = 2)

(* Test blit: into middle of destination removes overlapping chunks *)
let () =
  let src_chunk = mk_data ~length:50 [(0, 99)] in
  let src = mk_content (Some ()) [src_chunk] in
  let dst_chunk = mk_data ~length:200 [(0, 1); (100, 2); (150, 3)] in
  let dst = mk_content None [dst_chunk] in
  (* Blit 50 samples into position 50-100 *)
  (* The original chunk overlaps so it gets entirely removed *)
  Ffmpeg_content_base.blit ~copy src 0 dst 50 50;
  (* Only the new 50-sample chunk remains *)
  assert (Ffmpeg_content_base.length dst = 50)

(* Test stream boundaries: chunks with different stream_idx *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(0, 1); (50, 2)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(0, 3); (50, 4)] in
  let content = mk_content (Some ()) [chunk1; chunk2] in
  assert (Ffmpeg_content_base.length content = 200);
  (* Each chunk maintains its own stream_idx *)
  assert ((List.nth content.chunks 0).stream_idx = 1L);
  assert ((List.nth content.chunks 1).stream_idx = 2L)

(* Test blit preserves stream_idx in new chunks *)
let () =
  let src_chunk = mk_data ~stream_idx:42L ~length:100 [(0, 1)] in
  let src = mk_content (Some ()) [src_chunk] in
  let dst = mk_content None [] in
  Ffmpeg_content_base.blit ~copy src 0 dst 0 100;
  assert ((List.hd dst.chunks).stream_idx = 42L)

(* Test blit with multiple source chunks *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:50 [(0, 1); (25, 2)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:50 [(0, 3); (25, 4)] in
  let src = mk_content (Some ()) [chunk1; chunk2] in
  let dst = mk_content None [] in
  (* Blit entire content *)
  Ffmpeg_content_base.blit ~copy src 0 dst 0 100;
  assert (Ffmpeg_content_base.length dst = 100);
  (* Should have 2 chunks, one from each source chunk *)
  assert (List.length dst.chunks = 2)

(* Test blit spanning partial chunks *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(25, 1); (75, 2)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(25, 3); (75, 4)] in
  let src = mk_content (Some ()) [chunk1; chunk2] in
  let dst = mk_content None [] in
  (* Blit from 50 to 150 (spanning both chunks) *)
  Ffmpeg_content_base.blit ~copy src 50 dst 0 100;
  assert (Ffmpeg_content_base.length dst = 100);
  (* Should have 2 chunks *)
  assert (List.length dst.chunks = 2);
  (* First chunk: from position 50-100 of chunk1, contains position 75 -> adjusted to 25 *)
  let first = List.nth dst.chunks 0 in
  assert (first.stream_idx = 1L);
  assert (first.length = 50);
  assert (List.length first.data = 1);
  (* Second chunk: from position 0-50 of chunk2, contains position 25 -> stays at 25 *)
  let second = List.nth dst.chunks 1 in
  assert (second.stream_idx = 2L);
  assert (second.length = 50);
  assert (List.length second.data = 1)

(* Test concat via repeated blit *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(0, 1)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(0, 2)] in
  let src1 = mk_content (Some ()) [chunk1] in
  let src2 = mk_content (Some ()) [chunk2] in
  let dst = mk_content None [] in
  Ffmpeg_content_base.blit ~copy src1 0 dst 0 100;
  Ffmpeg_content_base.blit ~copy src2 0 dst 100 100;
  assert (Ffmpeg_content_base.length dst = 200);
  assert (List.length dst.chunks = 2);
  assert ((List.nth dst.chunks 0).stream_idx = 1L);
  assert ((List.nth dst.chunks 1).stream_idx = 2L)

(* Test blit overwrites overlapping destination chunks *)
let () =
  let dst_chunk = mk_data ~stream_idx:1L ~length:100 [(0, 1); (50, 2)] in
  let dst = mk_content None [dst_chunk] in
  let src_chunk = mk_data ~stream_idx:2L ~length:50 [(0, 99)] in
  let src = mk_content (Some ()) [src_chunk] in
  (* Blit into 25-75, overlapping the dst chunk *)
  Ffmpeg_content_base.blit ~copy src 0 dst 25 50;
  (* The original chunk overlaps with blit region [25,75) so it's removed *)
  (* Only the new 50-sample chunk remains *)
  assert (Ffmpeg_content_base.length dst = 50);
  assert ((List.hd dst.chunks).stream_idx = 2L)

(* Test empty content blit *)
let () =
  let src = mk_content (Some ()) [] in
  let dst = mk_content None [] in
  Ffmpeg_content_base.blit ~copy src 0 dst 0 0;
  assert (Ffmpeg_content_base.length dst = 0)

(* Test different time_bases are preserved *)
let () =
  let time_base1 = { Avutil.num = 1; den = 1000 } in
  let time_base2 = { Avutil.num = 1; den = 48000 } in
  let chunk1 = mk_data ~time_base:time_base1 ~length:100 [(0, 1)] in
  let chunk2 = mk_data ~time_base:time_base2 ~length:100 [(0, 2)] in
  let content = mk_content (Some ()) [chunk1; chunk2] in
  assert ((List.nth content.chunks 0).time_base = time_base1);
  assert ((List.nth content.chunks 1).time_base = time_base2)

(* Test sparse stream: multiple empty data chunks *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:1000 [] in
  let chunk2 = mk_data ~stream_idx:1L ~length:500 [] in
  let content = mk_content (Some ()) [chunk1; chunk2] in
  assert (Ffmpeg_content_base.length content = 1500);
  let copied = Ffmpeg_content_base.copy ~copy content in
  assert (Ffmpeg_content_base.length copied = 1500)

(* Test blit of sparse content *)
let () =
  let src_chunk = mk_data ~length:1000 [] in
  let src = mk_content (Some ()) [src_chunk] in
  let dst = mk_content None [] in
  Ffmpeg_content_base.blit ~copy src 0 dst 0 1000;
  assert (Ffmpeg_content_base.length dst = 1000);
  assert (List.length dst.chunks = 1);
  assert (List.length (List.hd dst.chunks).data = 0)

(* Test partial blit of sparse content *)
let () =
  let src_chunk = mk_data ~length:1000 [] in
  let src = mk_content (Some ()) [src_chunk] in
  let dst = mk_content None [] in
  Ffmpeg_content_base.blit ~copy src 200 dst 0 500;
  assert (Ffmpeg_content_base.length dst = 500);
  assert (List.length (List.hd dst.chunks).data = 0)

(* Test make_data helper *)
let () =
  let data = Ffmpeg_content_base.make_data () in
  assert (data.length = 0);
  assert (data.stream_idx = 0L);
  assert (data.time_base.Avutil.num = 1);
  assert (data.time_base.Avutil.den = 1);
  assert (data.data = [])

let () =
  let data = Ffmpeg_content_base.make_data ~length:500 ~stream_idx:42L () in
  assert (data.length = 500);
  assert (data.stream_idx = 42L)

let () = print_endline "All ffmpeg_content_base tests passed!"
