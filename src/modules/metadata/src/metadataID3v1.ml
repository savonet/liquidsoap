open MetadataBase
module R = Reader

let trim s =
  match String.index_opt s '\000' with Some n -> String.sub s 0 n | None -> s

(** Parse ID3v1 tags. *)
let parse ?(recode = MetadataCharEncoding.Naive.convert) f : metadata =
  let size = match R.size f with Some n -> n | None -> raise Invalid in
  R.drop f (size - 128);
  if R.read f 3 <> "TAG" then raise Invalid;
  let title = R.read f 30 |> trim |> recode ~source:`ISO_8859_1 in
  let artist = R.read f 30 |> trim |> recode ~source:`ISO_8859_1 in
  let album = R.read f 30 |> trim |> recode ~source:`ISO_8859_1 in
  let year = R.read f 4 |> trim |> recode ~source:`ISO_8859_1 in
  let comment = R.read f 30 in
  let comment, track, genre =
    if comment.[27] = '\000' then
      (trim comment, int_of_char comment.[28], int_of_char comment.[29])
    else (trim comment, 0, 0)
  in
  let comment = recode comment in
  let track = if track = 0 then "" else string_of_int track in
  let genre = if genre = 255 then "" else string_of_int genre in
  let genre = recode genre in
  [
    ("title", title);
    ("artist", artist);
    ("album", album);
    ("year", year);
    ("comment", comment);
    ("track", track);
    ("genre", genre);
  ]
  |> List.filter (fun (_, v) -> v <> "")

let parse_file ?custom_parser file = R.with_file ?custom_parser parse file
