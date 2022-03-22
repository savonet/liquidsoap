open MetadataBase

module R = Reader

let parse f : metadata =
  let len = R.int32_be f in
  if R.read f 4 <> "ftyp" then raise Invalid;
  R.drop f (len - 8);
  let ans = ref [] in
  let chunk () =
    let len = R.int32_be f in
    let tag = R.read f 4 in
    match tag with
    | _ -> R.drop f (len - 8)
  in
  try
    while true do
      chunk ()
    done;
    assert false
  with _ -> List.rev !ans
  
let parse_file = R.with_file parse
