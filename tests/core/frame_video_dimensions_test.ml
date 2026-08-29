(* Video dimensions are auto-detected from the first decoded file, so an
   [ideal_size] is only a default until they are first read. *)

let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_detect_dimensions#set true;
  assert (not Frame_settings.conf_video_width#is_set);
  assert (not Frame_settings.conf_video_height#is_set);
  let width, height = Frame.video_dimensions () in
  let width = Lazy.force width in
  let height = Lazy.force height in
  assert (0 < width && 0 < height);
  let ideal_size =
    { Frame.width = width + 320; height = height + 180; source = "test" }
  in
  let detected_width, detected_height = Frame.video_dimensions ~ideal_size () in
  assert (Lazy.force detected_width = width);
  assert (Lazy.force detected_height = height)
