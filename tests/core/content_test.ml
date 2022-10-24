open Content

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
