let p name m =
  let m = List.map (fun (l,v) -> "- " ^ l ^ " : " ^ v) m |> String.concat "\n" in
  Printf.printf "# Testing %s\n\n%s\n\n%!" name m

let () =
  p "mp3" (Metadata.ID3v2.parse_file "test.mp3");
  p "png" (Metadata.PNG.parse_file "test.png");
  p "jpg" (Metadata.JPEG.parse_file "test.jpg");
  p "avi" (Metadata.AVI.parse_file "test.avi")
