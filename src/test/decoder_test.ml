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
  Frame.allow_lazy_config_eval ();
  Audio_converter.Channel_layout.converters#register "native"
    channel_layout_converter

let () =
  assert (
    Decoder.can_decode_type
      { audio = 2; video = [||]; midi = 0 }
      { audio = 2; video = [||]; midi = 0 } );
  assert (
    Decoder.can_decode_type
      { audio = 1; video = [||]; midi = 0 }
      { audio = 2; video = [||]; midi = 0 } );
  assert (
    Decoder.can_decode_type
      { audio = 6; video = [||]; midi = 0 }
      { audio = 2; video = [||]; midi = 0 } );
  assert (
    not
      (Decoder.can_decode_type
         { audio = 1; video = [||]; midi = 0 }
         { audio = 2; video = [| (1920, 1080) |]; midi = 0 }) );
  assert (
    Decoder.can_decode_type
      { audio = 1; video = [| (1920, 1080) |]; midi = 0 }
      { audio = 2; video = [| (1920, 1080) |]; midi = 0 } );
  assert (
    not
      (Decoder.can_decode_type
         { audio = 1; video = [||]; midi = 0 }
         { audio = 2; video = [||]; midi = 1 }) );
  assert (
    Decoder.can_decode_type
      { audio = 2; video = [| (1920, 1080) |]; midi = 1 }
      { audio = 2; video = [||]; midi = 0 } );
  assert (
    Decoder.can_decode_type
      { audio = 2; video = [| (1920, 1080) |]; midi = 1 }
      { audio = 2; video = [| (1920, 1080) |]; midi = 1 } );
  assert (
    Decoder.can_decode_type
      { audio = 2; video = [| (1920, 1080) |]; midi = 0 }
      { audio = 0; video = [| (1920, 1080) |]; midi = 0 } )
