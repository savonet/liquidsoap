let () = Frame_settings.lazy_config_eval := true
let audio = Content.Audio.format_of_channels 2
let video = Content.default_format Content.Video.kind

let () =
  let buffer =
    Generator.create ~max_length:1000 (Frame.Fields.make ~audio ~video ())
  in
  Generator.put buffer Frame.Fields.audio (Content.make ~length:500 audio);
  assert (Generator.length buffer = 0);
  assert (Generator.buffered_length buffer = 500);
  Generator.put buffer Frame.Fields.video (Content.make ~length:250 video);
  assert (Generator.length buffer = 250);
  assert (Generator.buffered_length buffer = 500);
  let m = Frame.Metadata.from_list [("foo", "bla")] in

  Generator.add_metadata buffer m;
  Generator.add_track_mark buffer;

  Generator.put buffer Frame.Fields.video (Content.make ~length:1 video);
  let c = Generator.slice buffer (Generator.length buffer) in

  assert (
    Content.Metadata.get_data (Frame.Fields.find Frame.Fields.metadata c)
    = [(250, m)]);
  assert (
    Content.Track_marks.get_data (Frame.Fields.find Frame.Fields.track_marks c)
    = [250]);

  Generator.put buffer Frame.Fields.video (Content.make ~length:50 video);
  assert (Generator.length buffer = 50);

  (* Last position for length [n] is [n-1], same as with arrays.. *)
  Generator.add_metadata ~pos:23 buffer m;
  Generator.add_track_mark ~pos:23 buffer;

  let c = Generator.slice buffer 23 in

  assert (
    Content.Metadata.get_data (Frame.Fields.find Frame.Fields.metadata c) = []);
  assert (
    Content.Track_marks.get_data (Frame.Fields.find Frame.Fields.track_marks c)
    = []);

  assert (Generator.length buffer = 27);
  let c = Generator.slice buffer 1 in

  assert (
    Content.Metadata.get_data (Frame.Fields.find Frame.Fields.metadata c)
    = [(0, m)]);

  assert (
    Content.Track_marks.get_data (Frame.Fields.find Frame.Fields.track_marks c)
    = [0])

let () =
  let buffer =
    Generator.create ~max_length:1000 (Frame.Fields.make ~audio ())
  in
  Generator.put buffer Frame.Fields.audio (Content.make ~length:500 audio);
  assert (Generator.length buffer = 500);
  assert (Generator.buffered_length buffer = 500);

  let c = Generator.peek buffer in

  let m = Frame.Fields.find Frame.Fields.metadata c in
  assert (Content.length m = max_int);
  let slice = Content.sub m 34 234 in
  assert (Content.length slice = 234);
  let rem = Content.truncate m 23 in
  assert (Content.length rem = max_int);

  let m = Frame.Fields.find Frame.Fields.track_marks c in
  assert (Content.length m = max_int);
  let slice = Content.sub m 34 234 in
  assert (Content.length slice = 234);
  let rem = Content.truncate m 23 in
  assert (Content.length rem = max_int);

  Generator.truncate buffer 50;

  assert (Generator.length buffer = 450);
  assert (Generator.buffered_length buffer = 450);

  let c = Generator.peek buffer in

  let m = Frame.Fields.find Frame.Fields.metadata c in
  assert (Content.length m = max_int);

  let m = Frame.Fields.find Frame.Fields.track_marks c in
  assert (Content.length m = max_int);

  let c = Generator.slice buffer 100 in

  Frame.Fields.iter (fun _ c -> assert (Content.length c = 100)) c;
  assert (Generator.length buffer = 350);
  assert (Generator.buffered_length buffer = 350);

  (try
     Generator.put buffer Frame.Fields.video (Content.make ~length:250 video);
     assert false
   with Not_found -> ());
  try
    Generator.put buffer Frame.Fields.audio (Content.make ~length:250 video);
    assert false
  with Content.Invalid -> ()
