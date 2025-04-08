open MetadataBase
module R = Reader

let parse f : metadata =
  if R.read f 8 <> "\x89PNG\x0d\x0a\x1a\x0a" then raise Invalid;
  let _ = R.int32_be f in
  if R.read f 4 <> "IHDR" then raise Invalid;
  let width = R.int32_be f in
  let height = R.int32_be f in
  let bit_depth = R.byte f in
  let _ (* color_type *) = R.byte f in
  [
    ("width", string_of_int width);
    ("height", string_of_int height);
    ("bit_depth", string_of_int bit_depth);
  ]

let parse_file ?custom_parser file = R.with_file ?custom_parser parse file
