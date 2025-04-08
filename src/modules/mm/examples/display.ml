open Mm_image
module Img = Image.RGBA32

let read_PPM ?alpha fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let data = Bytes.create len in
  really_input ic data 0 len;
  close_in ic;
  Img.of_PPM ?alpha (Bytes.unsafe_to_string data)

let () =
  let fname = Sys.argv.(1) in
  let img = read_PPM fname in
  (* let img = Img.Scale.create ~kind:Img.Scale.Bilinear img 500 500 in *)
  let w, h = Img.dimensions img in
  Graphics.open_graph "";
  Graphics.resize_window w h;
  Graphics.draw_image (Graphics.make_image (Img.to_int_image img)) 0 0;
  ignore (Graphics.wait_next_event [Graphics.Key_pressed])
