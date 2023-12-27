let () =
  let pcm_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Typing.(pcm_t <: Lang.univ_t ())
