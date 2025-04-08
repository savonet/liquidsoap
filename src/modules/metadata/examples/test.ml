let p name m =
  let m =
    List.map (fun (l, v) -> "- " ^ l ^ " : " ^ v) m |> String.concat "\n"
  in
  Printf.printf "# Testing %s\n\n%s\n\n%!" name m

(* Test parsing of metadata. *)
let () =
  p "mp3v2" (Metadata.ID3v2.parse_file "test.mp3");
  p "mp3v1" (Metadata.ID3v1.parse_file "test.mp3");
  p "mp3" (Metadata.ID3.parse_file "test.mp3");
  p "wav" (Metadata.WAV.parse_file "test.wav");
  p "png" (Metadata.PNG.parse_file "test.png");
  p "jpg" (Metadata.JPEG.parse_file "test.jpg");
  p "avi" (Metadata.AVI.parse_file "test.avi");
  p "mp4" (Metadata.MP4.parse_file "test.mp4")

(* Test failures. *)
let () =
  Printf.printf "# Testing failures\n\n%!";
  let test t s f file =
    Printf.printf "- parsing %s as %s: %!" s t;
    try
      ignore (f file);
      assert false
    with Metadata.Invalid -> Printf.printf "failed as expected\n%!"
  in
  test "png" "jpg" Metadata.PNG.parse_file "test.jpg";
  test "jpg" "png" Metadata.JPEG.parse_file "test.png";
  test "mp3v2" "png" Metadata.ID3v2.parse_file "test.png";
  test "mp3v1" "png" Metadata.ID3v1.parse_file "test.png";
  test "mp3" "png" Metadata.ID3.parse_file "test.png";
  test "mp4" "mp3" Metadata.MP4.parse_file "test.mp3";
  test "avi" "mp3" Metadata.MP4.parse_file "test.avi"
