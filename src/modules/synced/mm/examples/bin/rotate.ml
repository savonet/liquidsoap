open Mm_image
module I = Image.YUV420

let update =
  let a = ref 0. in
  fun img ->
    a := !a +. 0.1;
    I.blank img;
    I.fill_alpha img 0;
    let r = I.create 200 100 in
    I.fill r (Image.Pixel.yuv_of_rgb (0xff, 0, 0));
    I.add r ~x:10 ~y:70 img;
    I.rotate (I.copy img) 200 200 !a img

let () =
  Printexc.record_backtrace true;
  let img = I.create 640 480 in
  let w, h = I.dimensions img in
  Graphics.open_graph "";
  Graphics.resize_window w h;
  Graphics.auto_synchronize false;
  while true do
    update img;
    Graphics.draw_image (Graphics.make_image (I.to_int_image img)) 0 0;
    Graphics.synchronize ();
    Unix.sleepf 0.1
  done;
  ignore (Graphics.wait_next_event [Graphics.Key_pressed])
