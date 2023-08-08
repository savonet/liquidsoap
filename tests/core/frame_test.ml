open Frame

let () =
  Frame_settings.lazy_config_eval := true;

  let src = create (Frame.Fields.make ()) in
  add_break src (Lazy.force size);
  let m = Frame.Metadata.from_list [("foo", "bar")] in
  set_all_metadata src [(0, m)];

  (* First check that last meta from src is
     set when dst does not have one. *)
  let dst = create (Frame.Fields.make ()) in
  add_break dst 1;
  get_chunk dst src;
  assert (get_all_metadata dst = [(1, m)]);

  (* Then check that is not set when it has
     one that is the same. *)
  let dst = create (Frame.Fields.make ()) in
  add_break dst 1;
  let m' = Frame.Metadata.from_list [("foo", "bar")] in
  set_all_metadata dst [(0, m')];
  get_chunk dst src;
  assert (get_all_metadata dst = [(0, m')]);

  (* Then check that it is set when it has one
     but it is different. *)
  let dst = create (Frame.Fields.make ()) in
  add_break dst 1;
  let m' = Frame.Metadata.from_list [("gni", "gno")] in
  set_all_metadata dst [(0, m')];
  get_chunk dst src;
  assert (get_all_metadata dst = [(0, m'); (1, m)])

let () =
  let pcm_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Typing.(pcm_t <: Lang.univ_t ())
