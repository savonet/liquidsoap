let p name m =
  let m = List.map (fun (l,v) -> "- " ^ l ^ " : " ^ v) m |> String.concat "\n" in
  Printf.printf "# Testing %s\n\n%s\n\n%!" name m

let () =
  p "mp3v2" (Metadata.ID3v2.parse_file "test.mp3");
  p "mp3v1" (Metadata.ID3v1.parse_file "test.mp3");
  p "mp3" (Metadata.ID3.parse_file "test.mp3");
  p "png" (Metadata.PNG.parse_file "test.png");
  p "jpg" (Metadata.JPEG.parse_file "test.jpg");
  p "avi" (Metadata.AVI.parse_file "test.avi");
  p "mp4" (Metadata.MP4.parse_file "test.mp4")
