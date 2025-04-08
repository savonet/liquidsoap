open Mm

let show img =
  let width = Image.YUV420.width img in
  let height = Image.YUV420.height img in
  let img = Image.YUV420.to_int_image img in
  Graphics.open_graph "";
  Graphics.resize_window width height;
  let img = Graphics.make_image img in
  Graphics.draw_image img 0 0;
  Graphics.synchronize ();
  Graphics.loop_at_exit [] (fun _ -> ())

let () =
  let width = 640 in
  let height = 480 in
  let img = Image.YUV420.create width height in
  Image.YUV420.blank img;
  Image.YUV420.fill img (Image.Pixel.yuv_of_rgb (0, 0, 0xff));
  Image.YUV420.fill_alpha img 0;
  let r = Image.YUV420.create 200 100 in
  Image.YUV420.fill r (Image.Pixel.yuv_of_rgb (0xff, 0, 0));
  Image.YUV420.add r ~x:10 ~y:70 img;
  if false then show img

let () =
  let module C = Image.CanvasYUV420 in
  let r = Image.YUV420.create 200 200 in
  Image.YUV420.fill r (Image.Pixel.yuv_of_rgb (0xff, 0, 0));
  let img = C.make ~x:150 ~y:200 ~width:600 ~height:600 r in
  let img = C.render ~transparent:true img in
  show img
