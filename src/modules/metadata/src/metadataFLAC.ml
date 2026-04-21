open MetadataBase
module R = Reader

let parse f : metadata =
  let id = R.read f 4 in
  if id <> "fLaC" then raise Invalid;
  let tags = ref [] in
  let rec block () =
    let n = R.uint8 f in
    let last = n land 0b10000000 <> 0 in
    let block_type = n land 0b01111111 in
    let len = R.int24_be f in
    (match block_type with
      | 4 ->
          (* Vorbis comment *)
          let n = ref 0 in
          let vendor_len = R.uint32_le f in
          let vendor = R.read f vendor_len in
          n := !n + 4 + vendor_len;
          tags := ("vendor", vendor) :: !tags;
          let list_len = R.uint32_le f in
          n := !n + 4;
          for _ = 1 to list_len do
            let len = R.uint32_le f in
            let tag = R.read f len in
            n := !n + 4 + len;
            match String.index_opt tag '=' with
              | Some k ->
                  let field = String.sub tag 0 k |> String.lowercase_ascii in
                  let value = String.sub tag (k + 1) (len - (k + 1)) in
                  tags := (field, value) :: !tags
              | None -> ()
          done;
          R.drop f (len - !n)
      | 6 -> (
          (* Picture *)
            match R.read_tag ~length:len ~label:"metadata_block_picture" f with
            | None -> ()
            | Some picture ->
                tags := ("metadata_block_picture", picture) :: !tags)
      | _ -> R.drop f len);
    if not last then block ()
  in
  block ();
  List.rev !tags

let parse_file ?custom_parser file = R.with_file ?custom_parser parse file

type picture = {
  picture_type : int;
  picture_mime : string;
  picture_description : string;
  picture_width : int;
  picture_height : int;
  picture_bpp : int;
  picture_colors : int;
  picture_data : string;
}

let parse_picture p =
  let n = ref 0 in
  let int () =
    let ans =
      (int_of_char p.[!n] lsl 24)
      + (int_of_char p.[!n + 1] lsl 16)
      + (int_of_char p.[!n + 2] lsl 8)
      + int_of_char p.[!n + 3]
    in
    n := !n + 4;
    ans
  in
  let string len =
    let ans = String.sub p !n len in
    n := !n + len;
    ans
  in
  let picture_type = int () in
  let mime_len = int () in
  let mime = string mime_len in
  let desc_len = int () in
  let desc = string desc_len in
  let width = int () in
  let height = int () in
  let bpp = int () in
  let colors = int () in
  let len = int () in
  let data = string len in
  {
    picture_type;
    picture_mime = mime;
    picture_description = desc;
    picture_width = width;
    picture_height = height;
    picture_bpp = bpp;
    picture_colors = colors;
    picture_data = data;
  }
