open MetadataBase
module R = Reader

let parse f : metadata =
  (* Start of image *)
  if R.read f 2 <> "\xff\xd8" then raise Invalid;
  let metadata = ref [] in
  let add l v = metadata := (l, v) :: !metadata in
  let rec read_maker () =
    if R.byte f <> 0xff then raise Invalid;
    let kind = R.byte f in
    let len = R.int16_be f in
    (* Printf.printf "Marker: %x (len: %d)\n%!" kind len; *)
      match kind with
      (* | 0xe0 -> *)
      (* Printf.printf "JFIF\n%!"; *)
      (* if R.read f 5 <> "JFIF\x00" then raise Invalid; *)
      (* let _ (\* major version *\) = R.byte f in *)
      (* let _ (\* minor version *\) = R.byte f in *)
      (* | 0xe1 -> *)
      (* Printf.printf "EXIF\n%!"; *)
      (* if R.read f 6 <> "Exif\x00\x00" then raise Invalid; *)
      (* let endianness = R.read f 2 in *)
      (* let endianness = if endianness = "MM" then Big_endian else if endianness = "II" then Little_endian else raise Invalid in *)
      (* let int16 = R.int16 endianness in *)
      (* if int16 f <> 0x2a then raise Invalid; *)
      (* let skip = int16 f in *)
      (* let _ = R.read f (skip - 8) in *)
      (* let entries = int16 f in *)
      | 0xc0 | 0xc2 ->
          let _ (* precision *) = R.byte f in
          let height = R.int16_be f in
          let width = R.int16_be f in
          add "width" (string_of_int width);
          add "height" (string_of_int height);
          (* Ignore after that. *)
          ()
      | _ ->
          let _ = R.read f (len - 2) in
          read_maker ()
  in
  read_maker ();
  List.rev !metadata

let parse_file ?custom_parser file = R.with_file ?custom_parser parse file
