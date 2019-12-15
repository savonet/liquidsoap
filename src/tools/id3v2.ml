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
  if enc = 0 then Configure.recode_tag ~in_enc:"ISO-8859-1" s
  else if enc = 1 || enc = 2 then Configure.recode_tag ~in_enc:"UTF-16" s
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
  (* Printf.printf "version: %s\n" (String.concat "." (List.map string_of_int (Array.to_list version))); *)
  let v = version.(1) in
  if v <> 3 && v <> 4 then raise Invalid;
  let flags = read_byte f in
  (* let unsynchronization = flags land 0b10000000 <> 0 in *)
  (* Printf.printf "unsynchronization: %b\n%!" unsynchronization; *)
  let extended_header = flags land 0b1000000 <> 0 in
  (* Printf.printf "extended header: %b\n" extended_header; *)
  let size = read_size f in
  (* Printf.printf "size: %d\n" size; *)
  if extended_header then (
    let size = read_size ~synch_safe:(v > 3) f in
    (* size *)
    let size = if v = 3 then size else size - 4 in
    ignore (read f size) );
  let len = ref size in
  let tags = ref [] in
  while !len > 0 do
    try
      (* Printf.printf "len: %d\n" !len; *)
      let id = read f 4 in
      (* Printf.printf "id: %s\n" id; *)
      if id = "\000\000\000\000" then len := 0 (* stop tag *)
      else (
        let size = read_size ~synch_safe:(v > 3) f in
        (* Printf.printf "size: %d\n" size; *)
        let flags = read f 2 in
        let data = read f size in
        (* if id <> "APIC" then Printf.printf "data: %S\n%!" data; *)
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
              | "TBPM" -> "bpm"
              | _ -> id
          in
          let encoding = int_of_char data.[0] in
          let z =
            if encoding = 0 then (
              try String.index_from data 1 '\000'
              with Not_found -> String.length data )
            else (* TODO: look for \000\000 *)
              String.length data
          in
          let text = String.sub data 1 (z - 1) in
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
  let recode = recode text_encoding in
  let n = String.index_from apic 1 '\000' in
  let mime = recode (String.sub apic 1 (n - 1)) in
  let n = n + 1 in
  let picture_type = int_of_char apic.[n] in
  let n = n + 1 in
  let n' = String.index_from apic n '\000' in
  let description = recode (String.sub apic n (n' - n)) in
  let n = n' + 1 in
  let data = String.sub apic n (String.length apic - n) in
  { mime; picture_type; description; data }
