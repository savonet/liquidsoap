let channel_layout_converter src dst =
  match (src, dst) with
    | `Mono, `Mono
    | `Mono, `Stereo
    | `Stereo, `Mono
    | `Stereo, `Stereo
    | `Five_point_one, `Five_point_one
    | `Five_point_one, `Stereo
    | `Five_point_one, `Mono ->
        fun x -> x
    | _ -> raise Audio_converter.Channel_layout.Unsupported

let () =
  Frame_settings.lazy_config_eval := true;
  Audio_converter.Channel_layout.converters#register "native"
    channel_layout_converter;
  Frame_settings.conf_video_default#set true

let () =
  let none = Content.None.format in
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
  let yuva420p = Content.default_video () in
  let midi = Content.(Midi.lift_params { Content.channels = 1 }) in
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:none ~midi:none ())
      (Frame.mk_fields ~audio:stereo ~video:none ~midi:none ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:mono ~video:none ~midi:none ())
      (Frame.mk_fields ~audio:stereo ~video:none ~midi:none ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:five_point_one ~video:none ~midi:none ())
      (Frame.mk_fields ~audio:stereo ~video:none ~midi:none ()));
  assert (
    not
      (Decoder.can_decode_type
         (Frame.mk_fields ~audio:mono ~video:none ~midi:none ())
         (Frame.mk_fields ~audio:stereo ~video:yuva420p ~midi:none ())));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:mono ~video:yuva420p ~midi:none ())
      (Frame.mk_fields ~audio:stereo ~video:yuva420p ~midi:none ()));
  assert (
    not
      (Decoder.can_decode_type
         (Frame.mk_fields ~audio:mono ~video:none ~midi:none ())
         (Frame.mk_fields ~audio:stereo ~video:none ~midi ())));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:yuva420p ~midi ())
      (Frame.mk_fields ~audio:stereo ~video:none ~midi ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:yuva420p ~midi ())
      (Frame.mk_fields ~audio:stereo ~video:yuva420p ~midi ()));
  assert (
    Decoder.can_decode_type
      (Frame.mk_fields ~audio:stereo ~video:yuva420p ~midi ())
      (Frame.mk_fields ~audio:none ~video:yuva420p ~midi ()))
