let () =
  let pcm_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Typing.(pcm_t <: Lang.univ_t ())

let () =
  let f = Frame.create ~length:10 (Frame.Fields.from_list []) in
  assert (Frame.position (Frame.slice f 20) = 10)
