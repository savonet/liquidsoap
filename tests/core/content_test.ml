open Content

let () = Frame_settings.allow_lazy_config_eval ()

let () =
  let marks ?(offset = 0) len = List.init len (fun x -> x + offset) in
  let c = Content.make ~length:1000 Content.Track_marks.format in
  let c' = Content.make ~length:10 Content.Track_marks.format in
  Track_marks.set_data c (marks 1000);
  (* Track marks outside of the declared length should be ignored. *)
  Track_marks.set_data c' (marks 10);
  assert (Track_marks.get_data c' = marks 10);
  assert (Track_marks.get_data (Content.sub c 5 10) = List.init 10 (fun x -> x));
  Track_marks.set_data c' [];
  Content.fill c' 0 c 5 10;
  assert (Track_marks.get_data c = marks 5 @ marks ~offset:15 (1000 - 15))

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
  let final = Content.make ~length Content.(default_format Video.kind) in
  Content.fill data 0 final 0 length;
  let data = Content.Video.get_data data in
  let final = Content.Video.get_data final in
  assert (List.length data.Content.Video.data = 1);
  assert (List.length final.Content.Video.data = 1);
  assert (
    compare_image
      (List.hd data.Content.Video.data)
      (List.hd final.Content.Video.data));
  let fst = Content.Video.get_data fst in
  assert (List.length fst.Content.Video.data = 1);
  assert (
    compare_image
      (List.hd fst.Content.Video.data)
      (List.hd final.Content.Video.data))

(* Another content test boundary.
   We create a source of 1 and a source of length 2 * Frame.size - 1
   and a destination of 2 * Frame.size and fill the source into destination.
   The second chunk should have enough data to fill the destination. *)
let () =
  let size = Lazy.force Frame.size in
  let src = Content.make ~length:1 Content.(default_format Video.kind) in
  let src =
    Content.append src
      (Content.make
         ~length:((2 * size) - 1)
         Content.(default_format Video.kind))
  in
  let dst =
    Content.make ~length:(2 * size) Content.(default_format Video.kind)
  in
  Content.fill src 0 dst 0 (2 * size);
  let src = Content.Video.get_data src in
  let dst = Content.Video.get_data dst in
  assert (
    List.length src.Content.Video.data = List.length dst.Content.Video.data);
  List.iteri
    (fun pos d ->
      assert (compare_image d (List.nth dst.Content.Video.data pos)))
    src.Content.Video.data
