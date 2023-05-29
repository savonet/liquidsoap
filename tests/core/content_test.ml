open Content

let () = Frame_settings.lazy_config_eval := true

let () =
  let marks ?(offset = 0) len = List.init len (fun x -> x + offset) in
  let c = Track_marks.(lift_data (make ~length:1000 ())) in
  let c' = Track_marks.(lift_data (make ~length:10 ())) in
  Track_marks.set_data c (marks 1000);
  (* Track marks outside of the declared length should be ignored. *)
  Track_marks.set_data c' (marks 10);
  assert (Track_marks.get_data c' = marks 10);
  assert (Track_marks.get_data (Content.sub c 5 10) = List.init 10 (fun x -> x));
  Track_marks.set_data c' [];
  Content.blit c' 0 c 5 10;
  assert (Track_marks.get_data c = marks 5 @ marks ~offset:15 (1000 - 15))

(* Test metadata uniqueness. *)
let () =
  let ctype =
    Frame_type.content_type
      (Lang.frame_t Lang.unit_t
         (Frame.Fields.make ~audio:(Format_type.audio ()) ()))
  in
  let frame = Frame.create ctype in
  let m = Hashtbl.create 12 in
  Hashtbl.add m "foo" "bla";
  Frame.set_metadata frame 123 m;
  let m = Hashtbl.create 12 in
  Hashtbl.add m "gni" "gno";
  Frame.set_metadata frame 123 m;
  assert (Frame.get_all_metadata frame = [(123, m)])

(* Test content boundaries.
   We create 3 content chunk and make
   sure that the consolidated content
   contains the last one's data. *)
let () =
  let length = Lazy.force Frame.size in
  let chunk_len = length / 3 in
  assert (Frame.video_of_main length = 1);
  let fst = Content.make ~length Content.(default_format Video.kind) in
  let fst = Content.sub fst 0 chunk_len in
  let snd = Content.make ~length Content.(default_format Video.kind) in
  let snd = Content.sub snd chunk_len chunk_len in
  let thrd_d = Content.make ~length Content.(default_format Video.kind) in
  let thrd = Content.sub thrd_d (2 * chunk_len) (length - (2 * chunk_len)) in
  let data = Content.append fst snd in
  let data = Content.append data thrd in
  assert (Content.length data = length);
  let final = Content.make ~length Content.(default_format Video.kind) in
  Content.blit data 0 final 0 length;
  let data = Content.Video.get_data data in
  let final = Content.Video.get_data final in
  assert (Array.length data = 1);
  assert (Array.length final = 1);
  assert (data.(0) == final.(0));
  let thrd_d = Content.Video.get_data thrd_d in
  assert (Array.length thrd_d = 1);
  assert (thrd_d.(0) == final.(0))

(* Another content test boundary.
   We create a source of 1 and a source of length 2 * Frame.size - 1
   and a destination of 2 * Frame.size and blit the source into destination.
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
  Content.blit src 0 dst 0 (2 * size);
  let src = Content.Video.get_data src in
  let dst = Content.Video.get_data dst in
  assert (Array.length src = Array.length dst);
  Array.iteri (fun pos d -> assert (d == dst.(pos))) src
