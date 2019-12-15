open Extralib

exception Invalid

let read f n =
  let s = Bytes.create n in
  let k = read_retry f s 0 n in
  if k <> n then raise Invalid;
  Bytes.unsafe_to_string s

let read_byte f = int_of_char (read f 1).[0]

let read_int f =
  let s = read f 4 in
  let s0 = int_of_char s.[0] in
  let s1 = int_of_char s.[1] in
  let s2 = int_of_char s.[2] in
  let s3 = int_of_char s.[3] in
  s3 lsl 24 + s2 lsl 16 + s1 lsl 8 + s0

let read_string f =
  let n = read_int f in
  read f n

let parse f =
  (* First packet. *)
  if read f 4 <> "OggS" then raise Invalid;
  let version = read_byte f in
  if version <> 0 then raise Invalid;
  let (* header_type *) _ = read_byte f in
  let (* granule_position *) _ = read f 8 in
  let (* bitstream_sn *) _ = read f 4 in
  let (* page_sn *) _ = read f 4 in
  let (* checksum *) _ = read f 4 in
  let lacing_len = read_byte f in
  let lacing = Array.init lacing_len (fun _ -> read_byte f) in
  Array.iter (fun n -> ignore (read f n)) lacing;

  (* Second packet. *)
  if read f 4 <> "OggS" then raise Invalid;
  let version = read_byte f in
  if version <> 0 then raise Invalid;
  let (* header_type *) _ = read_byte f in
  let (* granule_position *) _ = read f 8 in
  let (* bitstream_sn *) _ = read f 4 in
  let (* page_sn *) _ = read f 4 in
  let (* checksum *) _ = read f 4 in
  let lacing_len = read_byte f in
  let (* lacing *) _ = Array.init lacing_len (fun _ -> read_byte f) in
  let packet_type = read_byte f in
  if packet_type <> 3 then raise Invalid;
  let codec = read f 6 in
  if codec <> "vorbis" then raise Invalid;
  let vendor = read_string f in
  let n = read_int f in
  let ans = ref ["vendor", vendor] in
  for i = 0 to n-1 do
    let s = read_string f in
    let i = String.index s '=' in
    let name = String.sub s 0 i in
    let value = String.sub s (i+1) (String.length s - (i+1)) in
    let name =
      match name with
      | "ARTIST" -> "artist"
      | "TITLE" -> "title"
      | "TRACKNUMBER" -> "track"
      | "TEMPO" -> "tempo"
      | _ -> name
    in
    ans := (name, value) :: !ans
  done;
  !ans
