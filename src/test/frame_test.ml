open Frame
module None = Content.None

let () =
  Frame_base.lazy_config_eval := true;

  let src =
    create { audio = None.format; video = None.format; midi = None.format }
  in
  add_break src (Lazy.force size);
  let m = Hashtbl.create 1 in
  Hashtbl.add m "foo" "bar";
  set_all_metadata src [(0, m)];

  (* First check that last meta from src is
     set when dst does not have one. *)
  let dst =
    create { audio = None.format; video = None.format; midi = None.format }
  in
  add_break dst 1;
  get_chunk dst src;
  assert (get_all_metadata dst = [(1, m)]);

  (* Then check that is not set when it has
     one that is the same. *)
  let dst =
    create { audio = None.format; video = None.format; midi = None.format }
  in
  add_break dst 1;
  let m' = Hashtbl.create 1 in
  Hashtbl.add m' "foo" "bar";
  set_all_metadata dst [(0, m')];
  get_chunk dst src;
  assert (get_all_metadata dst = [(0, m')]);

  (* Then check that it is set when it has one
     but it is different. *)
  let dst =
    create { audio = None.format; video = None.format; midi = None.format }
  in
  add_break dst 1;
  let m' = Hashtbl.create 1 in
  Hashtbl.add m' "gni" "gno";
  set_all_metadata dst [(0, m')];
  get_chunk dst src;
  assert (get_all_metadata dst = [(0, m'); (1, m)])
