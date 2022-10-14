let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_default#set true

let () =
  let mono =
    Content.(Audio.lift_params { Content.channel_layout = lazy `Mono })
  in
  let stereo =
    Content.(Audio.lift_params { Content.channel_layout = lazy `Stereo })
  in
  let five_point_one =
    Content.(
      Audio.lift_params { Content.channel_layout = lazy `Five_point_one })
  in
  let canvas = Content.default_format Content_video.kind in
  let midi = Content.(Midi.lift_params { Content.channels = 1 }) in
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ())
      (Frame.mk_fields ~audio:stereo ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:mono ())
      (Frame.mk_fields ~audio:stereo ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:five_point_one ())
      (Frame.mk_fields ~audio:stereo ()));
  assert (
    not
      (Decoder.can_decode_type
         (Frame.mk_fields ~audio:mono ())
         (Frame.mk_fields ~audio:stereo ~video:canvas ())));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:mono ~video:canvas ())
      (Frame.mk_fields ~audio:stereo ~video:canvas ()));
  assert (
    not
      (Decoder.can_decode_type
         (Frame.mk_fields ~audio:mono ())
         (Frame.mk_fields ~audio:stereo ~midi ())));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:canvas ~midi ())
      (Frame.mk_fields ~audio:stereo ~midi ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:canvas ~midi ())
      (Frame.mk_fields ~audio:stereo ~video:canvas ~midi ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:canvas ~midi ())
      (Frame.mk_fields ~video:canvas ~midi ()))
