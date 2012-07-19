let decode_frame m =
  let s = Mad.decode_frame m in
  let f = Mad.get_frame_format m in
  let c = f.Mad.channels in
  let len = String.length s / (2 * c) in
  let ans = Array.init c (fun _ -> ABuf.create len) in
  ABuf.of_s16le s 0 len ans 0;
  ans
