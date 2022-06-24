open Content

let () =
  let marks ?(offset = 0) len = List.init len (fun x -> x + offset) in
  let c = TrackMark.(lift_data ~length:1000 (make ~length:1000 ())) in
  let c' = TrackMark.(lift_data ~length:10 (make ~length:10 ())) in
  TrackMark.set_data c (marks 1000);
  (* Track marks outside of the declared length should be ignored. *)
  TrackMark.set_data c' (marks 10);
  assert (TrackMark.get_data c' = marks 10);
  assert (TrackMark.get_data (Content.sub c 5 10) = List.init 10 (fun x -> x));
  TrackMark.set_data c' [];
  Content.blit c' 0 c 5 10;
  assert (TrackMark.get_data c = marks 5 @ marks ~offset:15 (1000 - 15))
