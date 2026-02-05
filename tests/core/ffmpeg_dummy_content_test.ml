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

(* Tests for high-level content operations using a dummy FFmpeg content module *)

let () =
  Frame_settings.conf_duration#set 0.04;
  Frame_settings.lazy_config_eval := true

(* Dummy packet type *)
type packet = int

(* Dummy params type *)
type params = { name : string }

(* Content type using Ffmpeg_content_base *)
type data = (params option, packet) Ffmpeg_content_base.content

let time_base = { Avutil.num = 1; den = 1000 }

(* Dummy Specs module *)
module DummySpecs = struct
  type kind = [ `Dummy ]
  type nonrec params = params option
  type nonrec data = data

  let name = "ffmpeg.dummy"
  let kind = `Dummy
  let parse_param _ _ = None
  let string_of_kind = function `Dummy -> "ffmpeg.dummy"
  let kind_of_string = function "ffmpeg.dummy" -> Some `Dummy | _ -> None

  let string_of_params = function
    | None -> "none"
    | Some p -> Printf.sprintf "name=%s" p.name

  let compatible p p' =
    match (p, p') with
      | None, _ | _, None -> true
      | Some p, Some p' -> p.name = p'.name

  let merge p p' =
    match (p, p') with
      | None, p | p, None -> p
      | p, p' when compatible p p' -> p
      | _ -> failwith "Incompatible format!"

  let length = Ffmpeg_content_base.length
  let params = Ffmpeg_content_base.params
  let copy_packet x = x

  let blit src src_pos dst dst_pos len =
    Ffmpeg_content_base.blit ~copy:copy_packet src src_pos dst dst_pos len

  let copy src = Ffmpeg_content_base.copy ~copy:copy_packet src
  let default_params _ = None
  let make ?length:_ params : data = Ffmpeg_content_base.make params

  let checksum (d : data) =
    let chunk_checksums =
      List.map
        (fun (data : packet Ffmpeg_content_base.data) ->
          let packets_data =
            List.map
              (fun (pos, packet) ->
                Printf.sprintf "%d:%Ld:%d/%d:%d" pos data.stream_idx
                  data.time_base.Avutil.num data.time_base.Avutil.den packet)
              data.data
          in
          String.concat "|" packets_data)
        d.chunks
    in
    Digest.string (String.concat "||" chunk_checksums) |> Digest.to_hex
end

(* Apply the Content functor *)
module DummyContent = Content.MkContent (DummySpecs)

(* Helper to create data with packets at specific positions *)
let mk_data ?(stream_idx = 1L) ?(time_base = time_base) ~length data =
  { Ffmpeg_content_base.length; stream_idx; time_base; data }

let mk_content params chunks : data = { Ffmpeg_content_base.params; chunks }

(* Test lift_data and get_data round-trip *)
let () =
  let chunk = mk_data ~stream_idx:42L ~length:100 [(0, 1); (50, 2)] in
  let content = mk_content (Some { name = "test" }) [chunk] in
  let lifted = DummyContent.lift_data content in
  let retrieved = DummyContent.get_data lifted in
  assert (Ffmpeg_content_base.length retrieved = 100);
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 42L);
  assert (c.length = 100);
  assert (c.time_base = time_base);
  assert (c.data = [(0, 1); (50, 2)])

(* Test lift_params and get_params round-trip *)
let () =
  let params = Some { name = "test_params" } in
  let lifted = DummyContent.lift_params params in
  let retrieved = DummyContent.get_params lifted in
  assert (retrieved = params)

(* Test Content.sub with offset and length *)
let () =
  let chunk =
    mk_data ~stream_idx:5L ~length:100 [(10, 1); (30, 2); (50, 3); (70, 4)]
  in
  let content = mk_content (Some { name = "test" }) [chunk] in
  let lifted = DummyContent.lift_data content in
  (* Sub from 20 to 60 (length 40) - includes positions 30 and 50 *)
  let sub = Content.sub lifted 20 40 in
  assert (Content.length sub = 40);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 5L);
  assert (c.length = 40);
  (* Positions 30 and 50 adjusted by -20 become 10 and 30 *)
  assert (c.data = [(10, 2); (30, 3)])

(* Test Content.sub preserves stream boundaries *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:50 [(10, 11); (30, 12)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:50 [(10, 21); (30, 22)] in
  let content = mk_content (Some { name = "test" }) [chunk1; chunk2] in
  let lifted = DummyContent.lift_data content in
  (* Sub spanning both chunks: positions 25-75 *)
  let sub = Content.sub lifted 25 50 in
  assert (Content.length sub = 50);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 2);
  (* First chunk: position 30 from chunk1, adjusted to 5 *)
  let c1 = List.nth retrieved.chunks 0 in
  assert (c1.stream_idx = 1L);
  assert (c1.length = 25);
  assert (c1.data = [(5, 12)]);
  (* Second chunk: position 10 from chunk2, adjusted to 10 (since chunk2 starts at pos 50, sub starts at 25, offset into chunk2 is 0) *)
  let c2 = List.nth retrieved.chunks 1 in
  assert (c2.stream_idx = 2L);
  assert (c2.length = 25);
  assert (c2.data = [(10, 21)])

(* Test Content.copy creates independent copy with same values *)
let () =
  let chunk = mk_data ~stream_idx:99L ~length:100 [(0, 42); (75, 99)] in
  let content = mk_content (Some { name = "original" }) [chunk] in
  let lifted = DummyContent.lift_data content in
  let copied = Content.copy lifted in
  assert (Content.length copied = 100);
  let retrieved = DummyContent.get_data copied in
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 99L);
  assert (c.length = 100);
  assert (c.data = [(0, 42); (75, 99)]);
  (* Modifying original shouldn't affect copy *)
  content.chunks <- [];
  assert (Content.length copied = 100)

(* Test Content.append concatenates content *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(0, 11); (50, 12)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(25, 21); (75, 22)] in
  let content1 = mk_content (Some { name = "first" }) [chunk1] in
  let content2 = mk_content (Some { name = "second" }) [chunk2] in
  let lifted1 = DummyContent.lift_data content1 in
  let lifted2 = DummyContent.lift_data content2 in
  let appended = Content.append lifted1 lifted2 in
  assert (Content.length appended = 200);
  let retrieved = DummyContent.get_data appended in
  assert (List.length retrieved.chunks = 2);
  let c1 = List.nth retrieved.chunks 0 in
  assert (c1.stream_idx = 1L);
  assert (c1.length = 100);
  assert (c1.data = [(0, 11); (50, 12)]);
  let c2 = List.nth retrieved.chunks 1 in
  assert (c2.stream_idx = 2L);
  assert (c2.length = 100);
  assert (c2.data = [(25, 21); (75, 22)])

(* Test Content.append with multiple stream boundaries *)
let () =
  let chunk1a = mk_data ~stream_idx:1L ~length:50 [(10, 1)] in
  let chunk1b = mk_data ~stream_idx:2L ~length:50 [(20, 2)] in
  let chunk2a = mk_data ~stream_idx:3L ~length:50 [(30, 3)] in
  let chunk2b = mk_data ~stream_idx:4L ~length:50 [(40, 4)] in
  let content1 = mk_content (Some { name = "first" }) [chunk1a; chunk1b] in
  let content2 = mk_content (Some { name = "second" }) [chunk2a; chunk2b] in
  let lifted1 = DummyContent.lift_data content1 in
  let lifted2 = DummyContent.lift_data content2 in
  let appended = Content.append lifted1 lifted2 in
  assert (Content.length appended = 200);
  let retrieved = DummyContent.get_data appended in
  assert (List.length retrieved.chunks = 4);
  assert ((List.nth retrieved.chunks 0).stream_idx = 1L);
  assert ((List.nth retrieved.chunks 0).data = [(10, 1)]);
  assert ((List.nth retrieved.chunks 1).stream_idx = 2L);
  assert ((List.nth retrieved.chunks 1).data = [(20, 2)]);
  assert ((List.nth retrieved.chunks 2).stream_idx = 3L);
  assert ((List.nth retrieved.chunks 2).data = [(30, 3)]);
  assert ((List.nth retrieved.chunks 3).stream_idx = 4L);
  assert ((List.nth retrieved.chunks 3).data = [(40, 4)])

(* Test empty data with positive length (sparse streams) *)
let () =
  let chunk = mk_data ~stream_idx:7L ~length:1000 [] in
  let content = mk_content (Some { name = "sparse" }) [chunk] in
  let lifted = DummyContent.lift_data content in
  assert (Content.length lifted = 1000);
  (* Sub should work on sparse content *)
  let sub = Content.sub lifted 100 500 in
  assert (Content.length sub = 500);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 7L);
  assert (c.length = 500);
  assert (c.data = [])

(* Test Content.is_empty *)
let () =
  let empty_content = mk_content None [] in
  let lifted = DummyContent.lift_data empty_content in
  assert (Content.is_empty lifted);
  let chunk = mk_data ~length:100 [] in
  let non_empty = mk_content None [chunk] in
  let lifted2 = DummyContent.lift_data non_empty in
  assert (not (Content.is_empty lifted2))

(* Test Content.truncate - drops first N samples *)
let () =
  let chunk = mk_data ~stream_idx:3L ~length:100 [(10, 1); (50, 2); (90, 3)] in
  let content = mk_content (Some { name = "test" }) [chunk] in
  let lifted = DummyContent.lift_data content in
  (* Truncate drops first 60 samples, leaving positions 60-100 *)
  let truncated = Content.truncate lifted 60 in
  assert (Content.length truncated = 40);
  let retrieved = DummyContent.get_data truncated in
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 3L);
  assert (c.length = 40);
  (* Only position 90 remains, adjusted to 30 (90 - 60) *)
  assert (c.data = [(30, 3)])

(* Test checksum changes with different packet values *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(0, 1)] in
  let chunk2 = mk_data ~stream_idx:1L ~length:100 [(0, 2)] in
  let content1 = mk_content None [chunk1] in
  let content2 = mk_content None [chunk2] in
  let lifted1 = DummyContent.lift_data content1 in
  let lifted2 = DummyContent.lift_data content2 in
  assert (Content.checksum lifted1 <> Content.checksum lifted2)

(* Test checksum is same for identical data *)
let () =
  let chunk = mk_data ~stream_idx:5L ~length:100 [(0, 42); (50, 99)] in
  let content = mk_content None [chunk] in
  let lifted = DummyContent.lift_data content in
  let copied = Content.copy lifted in
  assert (Content.checksum lifted = Content.checksum copied)

(* Test format compatibility *)
let () =
  let params1 = Some { name = "audio" } in
  let params2 = Some { name = "audio" } in
  let params3 = Some { name = "video" } in
  let format1 = DummyContent.lift_params params1 in
  let format2 = DummyContent.lift_params params2 in
  let format3 = DummyContent.lift_params params3 in
  assert (Content.compatible format1 format2);
  assert (not (Content.compatible format1 format3))

(* Test format with None is compatible with everything *)
let () =
  let params1 = None in
  let params2 = Some { name = "anything" } in
  let format1 = DummyContent.lift_params params1 in
  let format2 = DummyContent.lift_params params2 in
  assert (Content.compatible format1 format2);
  assert (Content.compatible format2 format1)

(* Test Content.make creates empty content *)
let () =
  let format = DummyContent.lift_params (Some { name = "new" }) in
  let content = Content.make ~length:500 format in
  let retrieved = DummyContent.get_data content in
  assert (Ffmpeg_content_base.length retrieved = 0);
  assert (retrieved.chunks = [])

(* Test multiple sub operations preserve stream boundaries and data *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(25, 11); (75, 12)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(10, 21); (75, 22)] in
  let content = mk_content None [chunk1; chunk2] in
  let lifted = DummyContent.lift_data content in
  (* First sub: 50-150 (spans both chunks) *)
  let sub1 = Content.sub lifted 50 100 in
  assert (Content.length sub1 = 100);
  (* After first sub:
     - From chunk1 (0-100): positions 50-100, so pos 75 -> 25
     - From chunk2 (100-200): positions 0-50, so pos 10 -> 10 *)
  (* Second sub of the result: 25-75 *)
  let sub2 = Content.sub sub1 25 50 in
  assert (Content.length sub2 = 50);
  let retrieved = DummyContent.get_data sub2 in
  assert (List.length retrieved.chunks = 2);
  (* From chunk1: position 75 -> after first sub at 25 -> after second sub at 0 *)
  let c1 = List.nth retrieved.chunks 0 in
  assert (c1.stream_idx = 1L);
  assert (c1.length = 25);
  assert (c1.data = [(0, 12)]);
  let c2 = List.nth retrieved.chunks 1 in
  assert (c2.stream_idx = 2L);
  assert (c2.length = 25);
  assert (c2.data = [(10, 21)])

(* Test lift_data with offset and length parameters *)
let () =
  let chunk =
    mk_data ~stream_idx:8L ~length:200 [(50, 1); (100, 2); (150, 3)]
  in
  let content = mk_content None [chunk] in
  (* Lift with offset=50 and length=100 means positions [50, 150) *)
  let lifted = DummyContent.lift_data ~offset:50 ~length:100 content in
  assert (Content.length lifted = 100);
  let retrieved = DummyContent.get_data lifted in
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 8L);
  assert (c.length = 100);
  (* Positions 50 and 100 are in range [50, 150), adjusted to 0 and 50 *)
  (* Position 150 is excluded (not < 150) *)
  assert (c.data = [(0, 1); (50, 2)])

(* Test time_base preservation through operations *)
let () =
  let time_base1 = { Avutil.num = 1; den = 48000 } in
  let time_base2 = { Avutil.num = 1; den = 44100 } in
  let chunk1 =
    mk_data ~stream_idx:1L ~time_base:time_base1 ~length:100 [(50, 1)]
  in
  let chunk2 =
    mk_data ~stream_idx:2L ~time_base:time_base2 ~length:100 [(50, 2)]
  in
  let content = mk_content None [chunk1; chunk2] in
  let lifted = DummyContent.lift_data content in
  let copied = Content.copy lifted in
  let retrieved = DummyContent.get_data copied in
  assert (List.length retrieved.chunks = 2);
  let c1 = List.nth retrieved.chunks 0 in
  assert (c1.stream_idx = 1L);
  assert (c1.time_base = time_base1);
  assert (c1.data = [(50, 1)]);
  let c2 = List.nth retrieved.chunks 1 in
  assert (c2.stream_idx = 2L);
  assert (c2.time_base = time_base2);
  assert (c2.data = [(50, 2)])

(* Test sub at exact chunk boundary *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(0, 1); (99, 2)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(0, 3); (99, 4)] in
  let content = mk_content None [chunk1; chunk2] in
  let lifted = DummyContent.lift_data content in
  (* Sub exactly the first chunk *)
  let sub = Content.sub lifted 0 100 in
  assert (Content.length sub = 100);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 1L);
  assert (c.length = 100);
  assert (c.data = [(0, 1); (99, 2)])

(* Test sub exactly the second chunk *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:100 [(0, 1)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:100 [(0, 2)] in
  let content = mk_content None [chunk1; chunk2] in
  let lifted = DummyContent.lift_data content in
  let sub = Content.sub lifted 100 100 in
  assert (Content.length sub = 100);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 1);
  let c = List.hd retrieved.chunks in
  assert (c.stream_idx = 2L);
  assert (c.length = 100);
  assert (c.data = [(0, 2)])

(* Test multiple stream_idx: interleaved streams *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:30 [(0, 1); (15, 2)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:40 [(5, 3); (25, 4)] in
  let chunk3 = mk_data ~stream_idx:1L ~length:30 [(10, 5)] in
  let content = mk_content None [chunk1; chunk2; chunk3] in
  let lifted = DummyContent.lift_data content in
  assert (Content.length lifted = 100);
  let retrieved = DummyContent.get_data lifted in
  assert (List.length retrieved.chunks = 3);
  (* Verify each chunk maintains its stream_idx *)
  assert ((List.nth retrieved.chunks 0).stream_idx = 1L);
  assert ((List.nth retrieved.chunks 1).stream_idx = 2L);
  assert ((List.nth retrieved.chunks 2).stream_idx = 1L);
  (* Verify data integrity *)
  assert ((List.nth retrieved.chunks 0).data = [(0, 1); (15, 2)]);
  assert ((List.nth retrieved.chunks 1).data = [(5, 3); (25, 4)]);
  assert ((List.nth retrieved.chunks 2).data = [(10, 5)])

(* Test multiple stream_idx: sub operation crossing stream boundaries *)
let () =
  let chunk1 = mk_data ~stream_idx:10L ~length:50 [(20, 1); (40, 2)] in
  let chunk2 = mk_data ~stream_idx:20L ~length:50 [(10, 3); (30, 4)] in
  let chunk3 = mk_data ~stream_idx:30L ~length:50 [(15, 5); (45, 6)] in
  let content = mk_content None [chunk1; chunk2; chunk3] in
  let lifted = DummyContent.lift_data content in
  (* Sub from 40 to 110 (length 70), crossing all three chunks *)
  let sub = Content.sub lifted 40 70 in
  assert (Content.length sub = 70);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 3);
  (* chunk1[40:50] -> 10 samples, pos 40 -> 0 *)
  let c1 = List.nth retrieved.chunks 0 in
  assert (c1.stream_idx = 10L);
  assert (c1.length = 10);
  assert (c1.data = [(0, 2)]);
  (* chunk2[0:50] -> 50 samples, positions stay *)
  let c2 = List.nth retrieved.chunks 1 in
  assert (c2.stream_idx = 20L);
  assert (c2.length = 50);
  assert (c2.data = [(10, 3); (30, 4)]);
  (* chunk3[0:10] -> 10 samples *)
  let c3 = List.nth retrieved.chunks 2 in
  assert (c3.stream_idx = 30L);
  assert (c3.length = 10);
  assert (c3.data = [])
(* positions 15 and 45 not in [0, 10) *)

(* Test multiple stream_idx: append preserves order *)
let () =
  let chunk_a1 = mk_data ~stream_idx:1L ~length:25 [(5, 11)] in
  let chunk_a2 = mk_data ~stream_idx:2L ~length:25 [(10, 12)] in
  let chunk_b1 = mk_data ~stream_idx:3L ~length:25 [(15, 21)] in
  let chunk_b2 = mk_data ~stream_idx:4L ~length:25 [(20, 22)] in
  let content_a = mk_content None [chunk_a1; chunk_a2] in
  let content_b = mk_content None [chunk_b1; chunk_b2] in
  let lifted_a = DummyContent.lift_data content_a in
  let lifted_b = DummyContent.lift_data content_b in
  let appended = Content.append lifted_a lifted_b in
  assert (Content.length appended = 100);
  let retrieved = DummyContent.get_data appended in
  assert (List.length retrieved.chunks = 4);
  (* Verify stream_idx order is preserved *)
  let stream_ids =
    List.map (fun c -> c.Ffmpeg_content_base.stream_idx) retrieved.chunks
  in
  assert (stream_ids = [1L; 2L; 3L; 4L]);
  (* Verify data values *)
  assert ((List.nth retrieved.chunks 0).data = [(5, 11)]);
  assert ((List.nth retrieved.chunks 1).data = [(10, 12)]);
  assert ((List.nth retrieved.chunks 2).data = [(15, 21)]);
  assert ((List.nth retrieved.chunks 3).data = [(20, 22)])

(* Test multiple stream_idx with different time_bases *)
let () =
  let tb1 = { Avutil.num = 1; den = 48000 } in
  let tb2 = { Avutil.num = 1; den = 44100 } in
  let tb3 = { Avutil.num = 1; den = 90000 } in
  let chunk1 = mk_data ~stream_idx:1L ~time_base:tb1 ~length:100 [(50, 1)] in
  let chunk2 = mk_data ~stream_idx:2L ~time_base:tb2 ~length:100 [(50, 2)] in
  let chunk3 = mk_data ~stream_idx:3L ~time_base:tb3 ~length:100 [(25, 3)] in
  let content = mk_content None [chunk1; chunk2; chunk3] in
  let lifted = DummyContent.lift_data content in
  (* Sub spanning all three: [50, 250) of [0, 300) *)
  let sub = Content.sub lifted 50 200 in
  assert (Content.length sub = 200);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 3);
  (* Verify each chunk keeps its time_base *)
  assert ((List.nth retrieved.chunks 0).time_base = tb1);
  assert ((List.nth retrieved.chunks 1).time_base = tb2);
  assert ((List.nth retrieved.chunks 2).time_base = tb3);
  (* Verify stream_idx preserved *)
  assert ((List.nth retrieved.chunks 0).stream_idx = 1L);
  assert ((List.nth retrieved.chunks 1).stream_idx = 2L);
  assert ((List.nth retrieved.chunks 2).stream_idx = 3L);
  (* Verify chunk lengths: chunk1[50:100]=50, chunk2[0:100]=100, chunk3[0:50]=50 *)
  assert ((List.nth retrieved.chunks 0).length = 50);
  assert ((List.nth retrieved.chunks 1).length = 100);
  assert ((List.nth retrieved.chunks 2).length = 50);
  (* Verify data positions adjusted correctly *)
  assert ((List.nth retrieved.chunks 0).data = [(0, 1)]);
  (* 50-50=0 *)
  assert ((List.nth retrieved.chunks 1).data = [(50, 2)]);
  (* stays at 50 within chunk *)
  assert ((List.nth retrieved.chunks 2).data = [(25, 3)])
(* 25 is in [0,50), stays at 25 *)

(* Test stream_idx: copy preserves all stream metadata *)
let () =
  let chunk1 = mk_data ~stream_idx:100L ~length:50 [(10, 1); (40, 2)] in
  let chunk2 = mk_data ~stream_idx:200L ~length:50 [(20, 3); (30, 4)] in
  let content = mk_content (Some { name = "multi" }) [chunk1; chunk2] in
  let lifted = DummyContent.lift_data content in
  let copied = Content.copy lifted in
  let retrieved = DummyContent.get_data copied in
  assert (List.length retrieved.chunks = 2);
  let c1 = List.nth retrieved.chunks 0 in
  let c2 = List.nth retrieved.chunks 1 in
  assert (c1.stream_idx = 100L);
  assert (c2.stream_idx = 200L);
  assert (c1.length = 50);
  assert (c2.length = 50);
  assert (c1.data = [(10, 1); (40, 2)]);
  assert (c2.data = [(20, 3); (30, 4)])

(* Test stream_idx: sparse streams with different stream_idx *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:500 [] in
  (* sparse audio *)
  let chunk2 = mk_data ~stream_idx:2L ~length:500 [(100, 1)] in
  (* sparse subtitle *)
  let chunk3 = mk_data ~stream_idx:1L ~length:500 [] in
  (* more sparse audio *)
  let content = mk_content None [chunk1; chunk2; chunk3] in
  let lifted = DummyContent.lift_data content in
  assert (Content.length lifted = 1500);
  (* Sub in the middle *)
  let sub = Content.sub lifted 400 700 in
  assert (Content.length sub = 700);
  let retrieved = DummyContent.get_data sub in
  assert (List.length retrieved.chunks = 3);
  (* chunk1[400:500] -> 100 samples, empty *)
  assert ((List.nth retrieved.chunks 0).stream_idx = 1L);
  assert ((List.nth retrieved.chunks 0).length = 100);
  assert ((List.nth retrieved.chunks 0).data = []);
  (* chunk2[0:500] -> 500 samples, has data at 100 *)
  assert ((List.nth retrieved.chunks 1).stream_idx = 2L);
  assert ((List.nth retrieved.chunks 1).length = 500);
  assert ((List.nth retrieved.chunks 1).data = [(100, 1)]);
  (* chunk3[0:100] -> 100 samples, empty *)
  assert ((List.nth retrieved.chunks 2).stream_idx = 1L);
  assert ((List.nth retrieved.chunks 2).length = 100);
  assert ((List.nth retrieved.chunks 2).data = [])

(* Test blit collapses adjacent chunks with same stream_idx *)
let () =
  (* Source has: stream_idx=1, stream_idx=1, stream_idx=2, stream_idx=2 *)
  let chunk1 = mk_data ~stream_idx:1L ~length:25 [(5, 11)] in
  let chunk2 = mk_data ~stream_idx:1L ~length:25 [(10, 12)] in
  let chunk3 = mk_data ~stream_idx:2L ~length:25 [(15, 21)] in
  let chunk4 = mk_data ~stream_idx:2L ~length:25 [(20, 22)] in
  let src = mk_content None [chunk1; chunk2; chunk3; chunk4] in
  let dst = mk_content None [] in
  DummySpecs.blit src 0 dst 0 100;
  (* After blit, should have 2 chunks: one for stream_idx=1, one for stream_idx=2 *)
  assert (List.length dst.chunks = 2);
  let c1 = List.nth dst.chunks 0 in
  assert (c1.stream_idx = 1L);
  assert (c1.length = 50);
  (* Data from chunk1 at 5 and chunk2 at 10 (offset by 25) = 35 *)
  assert (c1.data = [(5, 11); (35, 12)]);
  let c2 = List.nth dst.chunks 1 in
  assert (c2.stream_idx = 2L);
  assert (c2.length = 50);
  (* Data from chunk3 at 15 and chunk4 at 20 (offset by 25) = 45 *)
  assert (c2.data = [(15, 21); (45, 22)])

(* Test blit does NOT collapse non-adjacent chunks with same stream_idx *)
let () =
  (* Source has: stream_idx=1, stream_idx=2, stream_idx=1 (interleaved) *)
  let chunk1 = mk_data ~stream_idx:1L ~length:30 [(10, 11)] in
  let chunk2 = mk_data ~stream_idx:2L ~length:40 [(20, 21)] in
  let chunk3 = mk_data ~stream_idx:1L ~length:30 [(5, 12)] in
  let src = mk_content None [chunk1; chunk2; chunk3] in
  let dst = mk_content None [] in
  DummySpecs.blit src 0 dst 0 100;
  (* After blit, should have 3 chunks (not collapsed because not adjacent) *)
  assert (List.length dst.chunks = 3);
  assert ((List.nth dst.chunks 0).stream_idx = 1L);
  assert ((List.nth dst.chunks 0).length = 30);
  assert ((List.nth dst.chunks 0).data = [(10, 11)]);
  assert ((List.nth dst.chunks 1).stream_idx = 2L);
  assert ((List.nth dst.chunks 1).length = 40);
  assert ((List.nth dst.chunks 1).data = [(20, 21)]);
  assert ((List.nth dst.chunks 2).stream_idx = 1L);
  assert ((List.nth dst.chunks 2).length = 30);
  assert ((List.nth dst.chunks 2).data = [(5, 12)])

(* Test blit partial range collapses correctly *)
let () =
  let chunk1 = mk_data ~stream_idx:1L ~length:50 [(10, 11); (40, 12)] in
  let chunk2 = mk_data ~stream_idx:1L ~length:50 [(20, 13); (45, 14)] in
  let src = mk_content None [chunk1; chunk2] in
  let dst = mk_content None [] in
  (* Blit from position 30 to 80 (length 50) *)
  DummySpecs.blit src 30 dst 0 50;
  assert (List.length dst.chunks = 1);
  let c = List.hd dst.chunks in
  assert (c.stream_idx = 1L);
  assert (c.length = 50);
  (* chunk1[30:50] has pos 40 -> adjusted to 10
     chunk2[0:30] has pos 20 -> adjusted to 20+20=40 (dst offset 20) *)
  assert (c.data = [(10, 12); (40, 13)])

(* Test sequential blits to same dst collapse adjacent same-stream_idx chunks *)
let () =
  (* This tests the scenario where multiple blits are done sequentially,
     and chunks with the same stream_idx that become adjacent should be collapsed.
     This is the typical pattern when a generator does multiple blits to fill a frame. *)
  let src1 = mk_content None [mk_data ~stream_idx:1L ~length:100 [(10, 11)]] in
  let src2 = mk_content None [mk_data ~stream_idx:1L ~length:100 [(20, 12)]] in
  let dst = mk_content None [] in
  (* First blit: src1[80:100] -> dst[0:20] *)
  DummySpecs.blit src1 80 dst 0 20;
  (* After first blit: one chunk at position 0, length 20, no data (pos 10 is outside [80,100)) *)
  assert (List.length dst.chunks = 1);
  assert ((List.hd dst.chunks).length = 20);
  assert ((List.hd dst.chunks).data = []);
  (* Second blit: src2[0:80] -> dst[20:100] *)
  DummySpecs.blit src2 0 dst 20 80;
  (* After second blit: should have ONE collapsed chunk since both have stream_idx=1 *)
  assert (List.length dst.chunks = 1);
  let c = List.hd dst.chunks in
  assert (c.stream_idx = 1L);
  assert (c.length = 100);
  (* Data: src2's pos 20 is at dst position 20 + 20 = 40 *)
  assert (c.data = [(40, 12)])

(* Test sequential blits with different stream_idx do not collapse *)
let () =
  let src1 = mk_content None [mk_data ~stream_idx:1L ~length:100 [(10, 11)]] in
  let src2 = mk_content None [mk_data ~stream_idx:2L ~length:100 [(20, 12)]] in
  let dst = mk_content None [] in
  DummySpecs.blit src1 80 dst 0 20;
  DummySpecs.blit src2 0 dst 20 80;
  (* Should have TWO chunks since stream_idx differs *)
  assert (List.length dst.chunks = 2);
  assert ((List.nth dst.chunks 0).stream_idx = 1L);
  assert ((List.nth dst.chunks 0).length = 20);
  assert ((List.nth dst.chunks 1).stream_idx = 2L);
  assert ((List.nth dst.chunks 1).length = 80);
  assert ((List.nth dst.chunks 1).data = [(20, 12)])

let () = print_endline "All ffmpeg_dummy_content tests passed!"
