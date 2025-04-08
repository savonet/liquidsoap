open MetadataBase
module R = Reader

let read_size ~synch_safe f =
  let s = R.read f 4 in
  let s0 = int_of_char s.[0] in
  let s1 = int_of_char s.[1] in
  let s2 = int_of_char s.[2] in
  let s3 = int_of_char s.[3] in
  if synch_safe then (
    if s0 lor s1 lor s2 lor s3 land 0b10000000 <> 0 then raise Invalid;
    (s0 lsl 21) + (s1 lsl 14) + (s2 lsl 7) + s3)
  else (s0 lsl 24) + (s1 lsl 16) + (s2 lsl 8) + s3

let read_size_v2 f =
  let s = R.read f 3 in
  let s0 = int_of_char s.[0] in
  let s1 = int_of_char s.[1] in
  let s2 = int_of_char s.[2] in
  (s0 lsl 16) + (s1 lsl 8) + s2

(** Remove trailing nulls. *)
let unterminate encoding s =
  let n = String.length s in
  match encoding with
    | 0 | 3 ->
        if String.length s > 0 && s.[n - 1] = '\000' then String.sub s 0 (n - 1)
        else s
    | 1 | 2 ->
        if String.length s >= 2 && s.[n - 1] = '\000' && s.[n - 2] = '\000' then
          String.sub s 0 (n - 2)
        else s
    | _ -> failwith (Printf.sprintf "Unknown encoding: %d." encoding)

(** Find the index of the substring after the first null-terminated substring.
*)
let next_substring encoding ?(offset = 0) s =
  let ans = ref 0 in
  let utf16 = encoding = 1 || encoding = 2 in
  try
    if utf16 then
      for i = offset to (String.length s / 2) - 1 do
        if s.[2 * i] = '\000' && s.[(2 * i) + 1] = '\000' then (
          ans := (2 * i) + 2;
          raise Exit)
      done
    else
      for i = offset to String.length s - 1 do
        if s.[i] = '\000' then (
          ans := i + 1;
          raise Exit)
      done;
    raise Not_found
  with Exit -> !ans

let normalize_id = function
  | "COMM" -> "comment"
  | "TALB" -> "album"
  | "TBPM" -> "bpm"
  | "TCOM" -> "composer"
  | "TCON" -> "genre"
  | "TCOP" -> "copyright"
  | "TDAT" -> "date"
  | "TDOR" -> "original release time"
  | "TDRC" -> "recording time"
  | "TENC" -> "encodedby"
  | "TEXT" -> "lyricist"
  | "TIT1" -> "grouping"
  | "TIT2" -> "title"
  | "TIT3" -> "subtitle"
  | "TKEY" -> "key"
  | "TLAN" -> "language"
  | "TLEN" -> "length"
  | "TMED" -> "media"
  | "TOAL" -> "originalalbum"
  | "TOFN" -> "originalfilename"
  | "TOPE" -> "originalartist"
  | "TPOS" -> "discnumber"
  | "TPE1" -> "artist"
  | "TPE2" -> "albumartist"
  | "TPE3" -> "conductor"
  | "TPE4" -> "remixer"
  | "TPUB" -> "label"
  | "TRCK" -> "tracknumber"
  | "TSOA" -> "albumsort"
  | "TSO2" -> "albumartistsort"
  | "TSOT" -> "titlesort"
  | "TSRC" -> "isrc"
  | "TSSE" -> "encoder"
  | "TSST" -> "discsubtitle"
  | "TYER" -> "year"
  | "WOAR" -> "website"
  | "WXXX" -> "url"
  | id -> id

let make_recode recode =
  let recode =
    Option.value ~default:MetadataCharEncoding.Naive.convert recode
  in
  let recode : int -> string -> string = function
    | 0 -> recode ~source:`ISO_8859_1 ~target:`UTF_8
    | 1 -> recode ~source:`UTF_16 ~target:`UTF_8
    | 2 -> recode ~source:`UTF_16 ~target:`UTF_8
    | 3 -> recode ~source:`UTF_8 ~target:`UTF_8
    (* Invalid encoding. *)
    | _ -> fun s -> s
  in
  fun encoding s -> recode encoding (unterminate encoding s)

(** Parse ID3v2 tags. *)
let parse ?recode f : metadata =
  let recode = make_recode recode in
  let id = R.read f 3 in
  if id <> "ID3" then raise Invalid;
  let version =
    let v1 = R.byte f in
    let v2 = R.byte f in
    [| 2; v1; v2 |]
  in
  let v = version.(1) in
  if not (List.mem v [2; 3; 4]) then raise Invalid;
  let id_len, read_frame_size =
    if v = 2 then (3, read_size_v2) else (4, read_size ~synch_safe:(v > 3))
  in
  let flags = R.byte f in
  let unsynchronization = flags land 0b10000000 <> 0 in
  if unsynchronization then failwith "Unsynchronized headers not handled.";
  let extended_header = flags land 0b1000000 <> 0 in
  let size = read_size ~synch_safe:true f in
  let len = ref size in
  if extended_header then (
    let size = read_size ~synch_safe:(v > 3) f in
    let size = if v = 3 then size else size - 4 in
    len := !len - (size + 4);
    ignore (R.read f size));
  let tags = ref [] in
  while !len > 0 do
    try
      (* We can have 3 null bytes in the end even if id is 4 bytes. *)
      let id_len = min !len id_len in
      let id = R.read f (min !len id_len) in
      if id = "\000\000\000\000" || id = "\000\000\000" then len := 0
        (* stop tag *)
      else (
        let size = read_frame_size f in
        (* make sure that we remain within the bounds in case of a problem *)
        let size = min size (!len - 10) in
        let flags = if v = 2 then None else Some (R.read f 2) in
        let compressed =
          match flags with
            | None -> false
            | Some flags -> int_of_char flags.[1] land 0b10000000 <> 0
        in
        let encrypted =
          match flags with
            | None -> false
            | Some flags -> int_of_char flags.[1] land 0b01000000 <> 0
        in
        if compressed || encrypted then (
          ignore (R.read f size);
          len := !len - (size + 10);
          raise Exit);
        let tag = R.read_tag ~label:id ~length:size f in
        len := !len - (size + 10);
        if tag = None then raise Exit;
        let data = Option.get tag in
        let len = String.length data in
        if List.mem id ["SEEK"] then ()
        else if id = "TXXX" then (
          let encoding = int_of_char data.[0] in
          let data = String.sub data 1 (len - 1) in
          let recode = recode encoding in
          let id, data =
            let n = next_substring encoding data in
            (String.sub data 0 n, String.sub data n (String.length data - n))
          in
          let id = recode id in
          let data = recode data in
          tags := (id, data) :: !tags)
        else if id = "COMM" then (
          let encoding = int_of_char data.[0] in
          let recode = recode encoding in
          let data = String.sub data 1 (len - 1) in
          (* We ignore the language description of the comment. *)
          let n = try next_substring encoding data with Not_found -> 0 in
          let data = String.sub data n (String.length data - n) |> recode in
          tags := ("comment", data) :: !tags)
        else if id.[0] = 'T' then (
          let encoding = int_of_char data.[0] in
          let recode = recode encoding in
          let data = String.sub data 1 (len - 1) in
          if normalize_id id = "artist" then
            Printf.printf "Raw artist: %S\n%!" data;
          let data = data |> recode in
          if normalize_id id = "artist" then
            Printf.printf "Recoded artist: %S\n%!" data;
          tags := (normalize_id id, data) :: !tags)
        else tags := (normalize_id id, data) :: !tags)
    with Exit -> ()
  done;
  List.rev !tags

let parse_file ?recode ?custom_parser =
  R.with_file ?custom_parser (parse ?recode)

(** Dump ID3v2 header. *)
let dump f =
  let id = R.read f 3 in
  if id <> "ID3" then raise Invalid;
  let v1 = R.byte f in
  let _v2 = R.byte f in
  if not (List.mem v1 [2; 3; 4]) then raise Invalid;
  let _flags = R.byte f in
  let size = read_size ~synch_safe:true f in
  R.reset f;
  R.read f (10 + size)

let dump_file = R.with_file dump

(** APIC data. *)
type apic = {
  mime : string;
  picture_type : int;
  description : string;
  data : string;
}

type pic = {
  pic_format : string;
  pic_type : int;
  pic_description : string;
  pic_data : string;
}

(** Parse APIC data. *)
let parse_apic ?recode apic =
  let recode = make_recode recode in
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

let parse_pic ?recode pic =
  let recode = make_recode recode in
  let text_encoding = int_of_char pic.[0] in
  let text_bytes = if text_encoding = 1 || text_encoding = 2 then 2 else 1 in
  let recode = recode text_encoding in
  let pic_format = String.sub pic 1 3 in
  let pic_type = int_of_char pic.[4] in
  let l =
    Int.find (fun i ->
        i mod text_bytes = 0
        && pic.[5 + i] = '\000'
        && (text_bytes = 1 || pic.[5 + i + 1] = '\000'))
  in
  let pic_description = recode (String.sub pic 5 l) in
  let n = 5 + l + text_bytes in
  let pic_data = String.sub pic n (String.length pic - n) in
  { pic_format; pic_type; pic_description; pic_data }

type frame_id =
  [ `AENC
  | `APIC
  | `COMM
  | `COMR
  | `ENCR
  | `EQUA
  | `ETCO
  | `GEOB
  | `GRID
  | `IPLS
  | `LINK
  | `MCDI
  | `MLLT
  | `OWNE
  | `PCNT
  | `POPM
  | `POSS
  | `PRIV
  | `RBUF
  | `RVAD
  | `RVRB
  | `SYLT
  | `SYTC
  | `TALB
  | `TBPM
  | `TCOM
  | `TCON
  | `TCOP
  | `TDAT
  | `TDLY
  | `TENC
  | `TEXT
  | `TFLT
  | `TIME
  | `TIT1
  | `TIT2
  | `TIT3
  | `TKEY
  | `TLAN
  | `TLEN
  | `TMED
  | `TOAL
  | `TOFN
  | `TOLY
  | `TOPE
  | `TORY
  | `TOWN
  | `TPE1
  | `TPE2
  | `TPE3
  | `TPE4
  | `TPOS
  | `TPUB
  | `TRCK
  | `TRDA
  | `TRSN
  | `TRSO
  | `TSIZ
  | `TSRC
  | `TSSE
  | `TXXX
  | `TYER
  | `UFID
  | `USER
  | `USLT
  | `WCOM
  | `WCOP
  | `WOAF
  | `WOAR
  | `WOAS
  | `WORS
  | `WPAY
  | `WPUB
  | `WXXX ]

let binary_frame = function
  | `AENC | `ENCR | `EQUA | `ETCO | `GRID | `LINK | `MCDI | `MLLT | `PRIV
  | `PCNT | `POPM | `POSS | `RBUF | `RVAD | `RVRB | `SYTC | `UFID ->
      true
  | _ -> false

let frame_id_of_string = function
  | "AENC" -> Some `AENC
  | "APIC" -> Some `APIC
  | "COMM" -> Some `COMM
  | "COMR" -> Some `COMR
  | "ENCR" -> Some `ENCR
  | "EQUA" -> Some `EQUA
  | "ETCO" -> Some `ETCO
  | "GEOB" -> Some `GEOB
  | "GRID" -> Some `GRID
  | "IPLS" -> Some `IPLS
  | "LINK" -> Some `LINK
  | "MCDI" -> Some `MCDI
  | "MLLT" -> Some `MLLT
  | "OWNE" -> Some `OWNE
  | "PCNT" -> Some `PCNT
  | "POPM" -> Some `POPM
  | "POSS" -> Some `POSS
  | "PRIV" -> Some `PRIV
  | "RBUF" -> Some `RBUF
  | "RVAD" -> Some `RVAD
  | "RVRB" -> Some `RVRB
  | "SYLT" -> Some `SYLT
  | "SYTC" -> Some `SYTC
  | "TALB" -> Some `TALB
  | "TBPM" -> Some `TBPM
  | "TCOM" -> Some `TCOM
  | "TCON" -> Some `TCON
  | "TCOP" -> Some `TCOP
  | "TDAT" -> Some `TDAT
  | "TDLY" -> Some `TDLY
  | "TENC" -> Some `TENC
  | "TEXT" -> Some `TEXT
  | "TFLT" -> Some `TFLT
  | "TIME" -> Some `TIME
  | "TIT1" -> Some `TIT1
  | "TIT2" -> Some `TIT2
  | "TIT3" -> Some `TIT3
  | "TKEY" -> Some `TKEY
  | "TLAN" -> Some `TLAN
  | "TLEN" -> Some `TLEN
  | "TMED" -> Some `TMED
  | "TOAL" -> Some `TOAL
  | "TOFN" -> Some `TOFN
  | "TOLY" -> Some `TOLY
  | "TOPE" -> Some `TOPE
  | "TORY" -> Some `TORY
  | "TOWN" -> Some `TOWN
  | "TPE1" -> Some `TPE1
  | "TPE2" -> Some `TPE2
  | "TPE3" -> Some `TPE3
  | "TPE4" -> Some `TPE4
  | "TPOS" -> Some `TPOS
  | "TPUB" -> Some `TPUB
  | "TRCK" -> Some `TRCK
  | "TRDA" -> Some `TRDA
  | "TRSN" -> Some `TRSN
  | "TRSO" -> Some `TRSO
  | "TSIZ" -> Some `TSIZ
  | "TSRC" -> Some `TSRC
  | "TSSE" -> Some `TSSE
  | "TXXX" -> Some `TXXX
  | "TYER" -> Some `TYER
  | "UFID" -> Some `UFID
  | "USER" -> Some `USER
  | "USLT" -> Some `USLT
  | "WCOM" -> Some `WCOM
  | "WCOP" -> Some `WCOP
  | "WOAF" -> Some `WOAF
  | "WOAR" -> Some `WOAR
  | "WOAS" -> Some `WOAS
  | "WORS" -> Some `WORS
  | "WPAY" -> Some `WPAY
  | "WPUB" -> Some `WPUB
  | "WXXX" -> Some `WXXX
  | _ -> None

let string_of_frame_id = function
  | `AENC -> "AENC"
  | `APIC -> "APIC"
  | `COMM -> "COMM"
  | `COMR -> "COMR"
  | `ENCR -> "ENCR"
  | `EQUA -> "EQUA"
  | `ETCO -> "ETCO"
  | `GEOB -> "GEOB"
  | `GRID -> "GRID"
  | `IPLS -> "IPLS"
  | `LINK -> "LINK"
  | `MCDI -> "MCDI"
  | `MLLT -> "MLLT"
  | `OWNE -> "OWNE"
  | `PCNT -> "PCNT"
  | `POPM -> "POPM"
  | `POSS -> "POSS"
  | `PRIV -> "PRIV"
  | `RBUF -> "RBUF"
  | `RVAD -> "RVAD"
  | `RVRB -> "RVRB"
  | `SYLT -> "SYLT"
  | `SYTC -> "SYTC"
  | `TALB -> "TALB"
  | `TBPM -> "TBPM"
  | `TCOM -> "TCOM"
  | `TCON -> "TCON"
  | `TCOP -> "TCOP"
  | `TDAT -> "TDAT"
  | `TDLY -> "TDLY"
  | `TENC -> "TENC"
  | `TEXT -> "TEXT"
  | `TFLT -> "TFLT"
  | `TIME -> "TIME"
  | `TIT1 -> "TIT1"
  | `TIT2 -> "TIT2"
  | `TIT3 -> "TIT3"
  | `TKEY -> "TKEY"
  | `TLAN -> "TLAN"
  | `TLEN -> "TLEN"
  | `TMED -> "TMED"
  | `TOAL -> "TOAL"
  | `TOFN -> "TOFN"
  | `TOLY -> "TOLY"
  | `TOPE -> "TOPE"
  | `TORY -> "TORY"
  | `TOWN -> "TOWN"
  | `TPE1 -> "TPE1"
  | `TPE2 -> "TPE2"
  | `TPE3 -> "TPE3"
  | `TPE4 -> "TPE4"
  | `TPOS -> "TPOS"
  | `TPUB -> "TPUB"
  | `TRCK -> "TRCK"
  | `TRDA -> "TRDA"
  | `TRSN -> "TRSN"
  | `TRSO -> "TRSO"
  | `TSIZ -> "TSIZ"
  | `TSRC -> "TSRC"
  | `TSSE -> "TSSE"
  | `TXXX -> "TXXX"
  | `TYER -> "TYER"
  | `UFID -> "UFID"
  | `USER -> "USER"
  | `USLT -> "USLT"
  | `WCOM -> "WCOM"
  | `WCOP -> "WCOP"
  | `WOAF -> "WOAF"
  | `WOAR -> "WOAR"
  | `WOAS -> "WOAS"
  | `WORS -> "WORS"
  | `WPAY -> "WPAY"
  | `WPUB -> "WPUB"
  | `WXXX -> "WXXX"

type frame_flag =
  [ `Tag_alter_perservation of bool | `File_alter_preservation of bool ]

let default_flags = function
  | `AENC | `ETCO | `EQUA | `MLLT | `POSS | `SYLT | `SYTC | `RVAD | `TENC
  | `TLEN | `TSIZ ->
      [`Tag_alter_perservation true; `File_alter_preservation false]
  | _ -> [`Tag_alter_perservation true; `File_alter_preservation true]

type text_encoding = [ `ISO_8859_1 | `UTF_8 | `UTF_16 | `UTF_16LE | `UTF_16BE ]
type frame_data = [ `Text of text_encoding * string | `Binary of string ]
type frame = { id : frame_id; data : frame_data; flags : frame_flag list }

let write_string ~buf = Buffer.add_string buf
let write_int32 ~buf n = Buffer.add_int32_be buf (Int32.of_int n)
let write_int16 ~buf n = Buffer.add_int16_be buf n
let write_int ~buf n = Buffer.add_char buf (char_of_int n)

let write_size ~buf n =
  if 0x0fffffff < n then raise Invalid;
  for i = 0 to 3 do
    let n = n lsr (7 * (3 - i)) in
    Buffer.add_char buf (char_of_int (n land 0x7f))
  done

let render_frame_data ~version = function
  | `Binary b -> b
  | `Text (encoding, data) ->
      let encoding, data =
        match encoding with
          | `ISO_8859_1 -> (0, data)
          | `UTF_16 -> (1, data)
          | `UTF_16BE when version >= 3 -> (2, data)
          | `UTF_8 when version >= 3 -> (3, data)
          | source ->
              ( 1,
                MetadataCharEncoding.Naive.convert ~source ~target:`UTF_16 data
              )
      in
      Printf.sprintf "%c%s" (Char.chr encoding) data

let render_frames ~version frames =
  let buf = Buffer.create 1024 in
  List.iter
    (fun { id; data; flags } ->
      write_string ~buf (string_of_frame_id id);
      let data = render_frame_data ~version data in
      let frame_length = String.length data in
      if version < 4 then write_int32 ~buf frame_length
      else write_size ~buf frame_length;
      write_int16 ~buf
        (List.fold_left
           (fun flags flag ->
             flags
             lor
             match flag with
               | `Tag_alter_perservation true -> 0b10000000 lsl 8
               | `File_alter_preservation true -> 0b01000000 lsl 8
               | _ -> 0)
           0 flags);
      write_string ~buf data)
    frames;
  buf

let make ~version frames =
  let buf = Buffer.create 1024 in
  write_string ~buf "ID3";
  write_int ~buf version;
  write_int ~buf 0;
  let tags = 0 in
  write_int ~buf tags;
  let frame_content = render_frames ~version frames in
  write_size ~buf (Buffer.length frame_content);
  Buffer.add_buffer buf frame_content;
  Buffer.contents buf
