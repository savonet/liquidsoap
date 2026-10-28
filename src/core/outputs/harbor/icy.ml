let is_ogg_mime mime =
  let mime =
    match String.index_opt mime ';' with
      | Some i -> String.sub mime 0 i
      | None -> mime
  in
  match String.lowercase_ascii (String.trim mime) with
    | "application/ogg" | "audio/ogg" | "video/ogg" -> true
    | _ -> false

let negotiated ~mime headers =
  (not (is_ogg_mime mime))
  &&
    match List.assoc_opt "icy-metadata" headers with
    | None -> false
    | Some value -> (
        match int_of_string_opt (String.trim value) with
          | Some n -> n <> 0
          | None -> false)

type event = { generation : int; block : string }

let empty_block = "\000"
let max_text = 255 * 16
let max_url = 200
let ellipsis = "..."

(* Keeps at most [length] bytes, moving back to the start of a UTF-8 sequence
   when the value is valid UTF-8. *)
let cut value length =
  let length =
    if String.is_valid_utf_8 value then (
      let rec back i =
        if
          i > 0
          && i < String.length value
          && Char.code value.[i] land 0xC0 = 0x80
        then back (i - 1)
        else i
      in
      back length)
    else length
  in
  String.sub value 0 length

let truncate value limit =
  if String.length value <= limit then value
  else cut value (limit - String.length ellipsis) ^ ellipsis

let block ~url title =
  let url = Option.map (fun url -> truncate url max_url) url in
  let url_part =
    match url with Some url -> "StreamUrl='" ^ url ^ "';" | None -> ""
  in
  let title_part =
    match title with
      | None -> ""
      | Some title ->
          let overhead =
            String.length "StreamTitle='';" + String.length url_part
          in
          "StreamTitle='" ^ truncate title (max_text - overhead) ^ "';"
  in
  let text = title_part ^ url_part in
  let blocks = (String.length text + 15) / 16 in
  let block = Bytes.make (1 + (16 * blocks)) '\000' in
  Bytes.set block 0 (Char.chr blocks);
  Bytes.blit_string text 0 block 1 (String.length text);
  Bytes.unsafe_to_string block

let title metadata =
  match (List.assoc_opt "artist" metadata, List.assoc_opt "title" metadata) with
    | Some artist, Some title -> Some (artist ^ " - " ^ title)
    | Some artist, None -> Some artist
    | None, Some title -> Some title
    | None, None -> None

let event ~generation ~url metadata =
  { generation; block = block ~url (title metadata) }

let block_for ~last_generation = function
  | Some { generation; block } when generation <> last_generation ->
      (block, generation)
  | _ -> (empty_block, last_generation)

type position = { since_block : int; last_generation : int }

let start = { since_block = 0; last_generation = -1 }

let interleave ~metaint position stamp data =
  let output = Buffer.create (String.length data + 64) in
  let rec go position offset =
    if offset >= String.length data then position
    else (
      let position =
        if position.since_block < metaint then position
        else (
          let block, last_generation =
            block_for ~last_generation:position.last_generation stamp
          in
          Buffer.add_string output block;
          { since_block = 0; last_generation })
      in
      let length =
        min (String.length data - offset) (metaint - position.since_block)
      in
      Buffer.add_substring output data offset length;
      go
        { position with since_block = position.since_block + length }
        (offset + length))
  in
  let position = go position 0 in
  (Buffer.contents output, position)
