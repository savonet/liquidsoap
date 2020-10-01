open Frame

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
  let none = Frame_content.None.format in
  let mono =
    Frame_content.(Audio.lift_params { Contents.channel_layout = lazy `Mono })
  in
  let stereo =
    Frame_content.(Audio.lift_params { Contents.channel_layout = lazy `Stereo })
  in
  let five_point_one =
    Frame_content.(
      Audio.lift_params { Contents.channel_layout = lazy `Five_point_one })
  in
  let yuv420p = Frame_content.default_video () in
  let midi = Frame_content.(Midi.lift_params { Contents.channels = 1 }) in
  assert (
    Decoder.can_decode_type
      { audio = stereo; video = none; midi = none }
      { audio = stereo; video = none; midi = none } );
  assert (
    Decoder.can_decode_type
      { audio = mono; video = none; midi = none }
      { audio = stereo; video = none; midi = none } );
  assert (
    Decoder.can_decode_type
      { audio = five_point_one; video = none; midi = none }
      { audio = stereo; video = none; midi = none } );
  assert (
    not
      (Decoder.can_decode_type
         { audio = mono; video = none; midi = none }
         { audio = stereo; video = yuv420p; midi = none }) );
  assert (
    Decoder.can_decode_type
      { audio = mono; video = yuv420p; midi = none }
      { audio = stereo; video = yuv420p; midi = none } );
  assert (
    not
      (Decoder.can_decode_type
         { audio = mono; video = none; midi = none }
         { audio = stereo; video = none; midi }) );
  assert (
    Decoder.can_decode_type
      { audio = stereo; video = yuv420p; midi }
      { audio = stereo; video = none; midi = none } );
  assert (
    Decoder.can_decode_type
      { audio = stereo; video = yuv420p; midi }
      { audio = stereo; video = yuv420p; midi } );
  assert (
    Decoder.can_decode_type
      { audio = stereo; video = yuv420p; midi = none }
      { audio = none; video = yuv420p; midi = none } )
