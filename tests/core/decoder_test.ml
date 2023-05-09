let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_default#set true

let () =
  let mono =
    Content.(
      Audio.lift_params { Content.channel_layout = SyncLazy.from_val `Mono })
  in
  let stereo =
    Content.(
      Audio.lift_params { Content.channel_layout = SyncLazy.from_val `Stereo })
  in
  let five_point_one =
    Content.(
      Audio.lift_params
        { Content.channel_layout = SyncLazy.from_val `Five_point_one })
  in
  let canvas = Content.default_format Content_video.kind in
  let midi = Content.(Midi.lift_params { Content.channels = 1 }) in
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:stereo ())
      (Frame.Fields.make ~audio:stereo ()));
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:mono ())
      (Frame.Fields.make ~audio:stereo ()));
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:five_point_one ())
      (Frame.Fields.make ~audio:stereo ()));
  assert (
    not
      (Decoder.can_decode_type
         (Frame.Fields.make ~audio:mono ())
         (Frame.Fields.make ~audio:stereo ~video:canvas ())));
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:mono ~video:canvas ())
      (Frame.Fields.make ~audio:stereo ~video:canvas ()));
  assert (
    not
      (Decoder.can_decode_type
         (Frame.Fields.make ~audio:mono ())
         (Frame.Fields.make ~audio:stereo ~midi ())));
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:stereo ~video:canvas ~midi ())
      (Frame.Fields.make ~audio:stereo ~midi ()));
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:stereo ~video:canvas ~midi ())
      (Frame.Fields.make ~audio:stereo ~video:canvas ~midi ()));
  assert (
    Decoder.can_decode_type
      (Frame.Fields.make ~audio:stereo ~video:canvas ~midi ())
      (Frame.Fields.make ~video:canvas ~midi ()))
