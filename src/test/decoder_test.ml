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
    Decoder.can_decode_kind
      { audio = 2; video = 0; midi = 0 }
      { audio = Succ (Succ Zero); video = Zero; midi = Zero } );
  assert (
    Decoder.can_decode_kind
      { audio = 1; video = 0; midi = 0 }
      { audio = Succ (Succ Zero); video = Zero; midi = Zero } );
  assert (
    Decoder.can_decode_kind
      { audio = 6; video = 0; midi = 0 }
      { audio = Succ (Succ Zero); video = Zero; midi = Zero } );
  assert (
    not
      (Decoder.can_decode_kind
         { audio = 1; video = 0; midi = 0 }
         { audio = Succ (Succ Zero); video = Succ Zero; midi = Zero }) );
  assert (
    Decoder.can_decode_kind
      { audio = 1; video = 1; midi = 0 }
      { audio = Succ (Succ Zero); video = Succ Zero; midi = Zero } );
  assert (
    not
      (Decoder.can_decode_kind
         { audio = 1; video = 0; midi = 0 }
         { audio = Succ (Succ Zero); video = Any; midi = Succ Zero }) );
  assert (
    Decoder.can_decode_kind
      { audio = 2; video = 1; midi = 1 }
      { audio = Succ (Succ Zero); video = Zero; midi = Zero } );
  assert (
    Decoder.can_decode_kind
      { audio = 2; video = 1; midi = 1 }
      { audio = Succ (Succ Zero); video = Any; midi = Any } );
  assert (
    Decoder.can_decode_kind
      { audio = 2; video = 1; midi = 0 }
      { audio = Zero; video = Succ Zero; midi = Zero } )
