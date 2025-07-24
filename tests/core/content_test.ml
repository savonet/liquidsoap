open Content

let () =
  Frame_settings.conf_duration#set 0.04;
  Frame_settings.lazy_config_eval := true

let () =
  let marks ?(offset = 0) len = List.init len (fun x -> x + offset) in
  let c = Content.make ~length:1000 Content.Track_marks.format in
  let c' = Content.make ~length:10 Content.Track_marks.format in
  Track_marks.set_data c (marks 1000);
  (* Track marks outside of the declared length should be ignored. *)
  Track_marks.set_data c' (marks 10);
  assert (Track_marks.get_data c' = marks 10);
  assert (Track_marks.get_data (Content.sub c 5 10) = List.init 10 (fun x -> x))

(* Test metadata uniqueness. *)
let () =
  let ctype =
    Frame_type.content_type
      (Lang.frame_t Lang.unit_t
         (Frame.Fields.make ~audio:(Format_type.audio ()) ()))
  in
  let frame = Frame.create ~length:(Lazy.force Frame.size) ctype in
  let m = Frame.Metadata.from_list [("foo", "bla")] in
  let frame = Frame.add_metadata frame 123 m in
  let m = Frame.Metadata.from_list [("gni", "gno")] in
  let frame = Frame.add_metadata frame 123 m in
  assert (Frame.get_all_metadata frame = [(123, m)])

let compare_image (p, img) (p', img') = p = p' && img == img'

(* Test content boundaries.
   We create 3 content chunk and make
   sure that the consolidated content
   contains the first one's data. *)
let () =
  let length = Lazy.force Frame.size in
  let chunk_len = length / 3 in
  assert (Frame.video_of_main length = 1);
  let fst = Content.make ~length Content.(default_format Video.kind) in
  let fst = Content.sub fst 0 chunk_len in
  let snd = Content.make ~length Content.(default_format Video.kind) in
  let snd = Content.sub snd chunk_len chunk_len in
  let thrd = Content.make ~length Content.(default_format Video.kind) in
  let thrd = Content.sub thrd (2 * chunk_len) (length - (2 * chunk_len)) in
  let data = Content.append fst snd in
  let data = Content.append data thrd in
  assert (Content.length data = length);
  let data = Content.Video.get_data data in
  assert (List.length data.Content.Video.data = 1);
  let fst = Content.Video.get_data fst in
  assert (List.length fst.Content.Video.data = 1);
  assert (
    compare_image
      (List.hd fst.Content.Video.data)
      (List.hd data.Content.Video.data))
