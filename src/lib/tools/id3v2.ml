open Extralib

exception Invalid

let read f n =
  let s = Bytes.create n in
  let k = read_retry f s 0 n in
  if k <> n then raise Invalid;
  Bytes.unsafe_to_string s

let read_byte f = int_of_char (read f 1).[0]

let read_size ?(synch_safe = true) f =
  let s = read f 4 in
  let s0 = int_of_char s.[0] in
  let s1 = int_of_char s.[1] in
  let s2 = int_of_char s.[2] in
  let s3 = int_of_char s.[3] in
  if synch_safe then (
    if s0 lor s1 lor s2 lor s3 land 0b10000000 <> 0 then raise Invalid;
    (s0 lsl 21) + (s1 lsl 14) + (s2 lsl 7) + s3 )
  else (s0 lsl 24) + (s1 lsl 16) + (s2 lsl 8) + s3

let recode enc s =
  if enc = 0 then Camomile_utils.recode_tag ~in_enc:"ISO-8859-1" s
  else if enc = 1 || enc = 2 then Camomile_utils.recode_tag ~in_enc:"UTF-16" s
  else if enc = 3 then s
  else s

let parse f =
  let id = read f 3 in
  if id <> "ID3" then raise Invalid;
  let version =
    let v1 = read_byte f in
    let v2 = read_byte f in
    [| 2; v1; v2 |]
  in
  let v = version.(1) in
  if v <> 3 && v <> 4 then raise Invalid;
  let flags = read_byte f in
  let extended_header = flags land 0b1000000 <> 0 in
  let size = read_size f in
  if extended_header then (
    let size = read_size ~synch_safe:(v > 3) f in
    (* size *)
    let size = if v = 3 then size else size - 4 in
    ignore (read f size) );
  let len = ref size in
  let tags = ref [] in
  while !len > 0 do
    try
      let id = read f 4 in
      if id = "\000\000\000\000" then len := 0 (* stop tag *)
      else (
        let size = read_size ~synch_safe:(v > 3) f in
        let flags = read f 2 in
        let data = read f size in
        len := !len - (size + 10);
        let compressed = int_of_char flags.[1] land 0b10000000 <> 0 in
        let encrypted = int_of_char flags.[1] land 0b01000000 <> 0 in
        if compressed || encrypted then raise Exit;
        if id.[0] = 'T' then (
          let id =
            match id with
              | "COMM" -> "comment"
              | "TPE1" -> "artist"
              | "TIT2" -> "title"
              | "TALB" -> "album"
              | "TYER" -> "year"
              | "TRCK" -> "track"
              | "TBPM" -> "tempo"
              | _ -> id
          in
          let encoding = int_of_char data.[0] in
          let start, len =
            match encoding with
              | 0x00 (* ISO-8859-1 *) | 0x03 (* UTF8 *) ->
                  if data.[size - 1] = '\000' then (1, size - 2)
                  else (1, size - 1)
              | 0x01 (* 16-bit unicode 2.0 *) | 0x02 (* UTF-16BE *) ->
                  if
                    size >= 2
                    && data.[size - 2] = '\000'
                    && data.[size - 1] = '\000'
                  then (1, size - 3)
                  else (1, size - 1)
              | _ -> (0, size)
          in
          let text = String.sub data start len in
          let text = recode encoding text in
          tags := (id, text) :: !tags )
        else tags := (id, data) :: !tags )
    with Exit -> ()
  done;
  !tags

type apic = {
  mime : string;
  picture_type : int;
  description : string;
  data : string;
}

let parse_apic apic =
  let text_encoding = int_of_char apic.[0] in
  let text_bytes = if text_encoding = 1 || text_encoding = 2 then 2 else 1 in
  let recode = recode text_encoding in
  let n = String.index_from apic 1 '\000' in
  let mime = String.sub apic 1 (n - 1) in
  let n = n + 1 in
  let picture_type = int_of_char apic.[n] in
  let n = n + 1 in
  let l =
    Int.find (fun i ->
        i mod text_bytes = 0
        && apic.[n + i] = '\000'
        && (text_bytes = 1 || apic.[n + i + 1] = '\000'))
  in
  let description = recode (String.sub apic n l) in
  let n = n + l + text_bytes in
  let data = String.sub apic n (String.length apic - n) in
  { mime; picture_type; description; data }
