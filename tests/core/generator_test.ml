module G = Generator

let () =
  Frame_base.lazy_config_eval := true;
  let frame_size = Lazy.force Frame.size in
  let gen = G.create `Both in
  let put_audio ~pts gen ofs len =
    let data = Content.None.data len in
    G.put_audio ~pts gen data ofs len
  in
  let put_video ~pts gen ofs len =
    let data = Content.None.data len in
    G.put_video ~pts gen data ofs len
  in
  (* Set this:
     0----1----2--> audio
     0----1----2----3----4----> video *)
  put_audio ~pts:0L gen 0 frame_size;
  put_audio ~pts:1L gen 0 frame_size;
  put_audio ~pts:2L gen 0 (frame_size / 2);
  assert (G.video_length gen = 0);
  assert (G.audio_length gen = (2 * frame_size) + (frame_size / 2));
  assert (G.length gen = 0);

  put_video ~pts:0L gen 0 frame_size;
  put_video ~pts:1L gen 0 frame_size;
  put_video ~pts:2L gen 0 frame_size;
  put_video ~pts:3L gen 0 (2 * frame_size);
  assert (G.video_length gen = 5 * frame_size);
  assert (G.audio_length gen = (2 * frame_size) + (frame_size / 2));
  assert (G.length gen = 2 * frame_size);

  (* Add 2--(3)----(4)-- audio *)
  put_audio ~pts:2L gen 0 (2 * frame_size);
  (* Get:
     0----1----2----3----4--> audio
     0----1----2----3----4----> video *)
  assert (G.video_length gen = 5 * frame_size);
  assert (G.audio_length gen = (4 * frame_size) + (frame_size / 2));
  assert (G.length gen = 4 * frame_size);

  (* Add 1---- video (non-monotonic PTS) *)
  put_video ~pts:1L gen 0 frame_size;
  (* Get:
     0----1----2----3----4--> audio
     0----1----2----3----4----> video *)
  assert (G.video_length gen = 5 * frame_size);
  assert (G.audio_length gen = (4 * frame_size) + (frame_size / 2));
  assert (G.length gen = 4 * frame_size);

  (* Add 4--(5)-- audio *)
  put_audio ~pts:4L gen 0 frame_size;
  (* Get:
     0----1----2----3----4----5--> audio
     0----1----2----3----4----> video *)
  assert (G.video_length gen = 5 * frame_size);
  assert (G.audio_length gen = (5 * frame_size) + (frame_size / 2));
  assert (G.length gen = 5 * frame_size);

  (* Add 6---- video (discontinuity) *)
  put_video ~pts:6L gen 0 frame_size;
  (* Get:
     0----1----2----3----4----5--> audio
     0----1----2----3----4----6----> video *)
  assert (G.video_length gen = 6 * frame_size);
  assert (G.audio_length gen = (5 * frame_size) + (frame_size / 2));
  assert (G.length gen = 5 * frame_size);

  (* Add 5--(6)-- audio (partial out-of-sync) *)
  put_audio ~pts:5L gen 0 frame_size;
  (* Get:
     0----1----2----3----4----6--> audio
     0----1----2----3----4----6----> video *)
  assert (G.video_length gen = 6 * frame_size);
  assert (G.audio_length gen = (5 * frame_size) + (frame_size / 2));
  assert (G.length gen = 5 * frame_size);

  (* Add 7----(8)-- audio *)
  put_audio ~pts:7L gen 0 (3 * frame_size / 2);
  (* Get:
     0----1----2----3----4----7----8--> audio
     0----1----2----3----4----> video *)
  assert (G.video_length gen = 5 * frame_size);
  assert (G.audio_length gen = (6 * frame_size) + (frame_size / 2));
  assert (G.length gen = 5 * frame_size);

  (* Add 9-- audio (discontinuity) *)
  put_audio ~pts:9L gen 0 (frame_size / 2);
  (* Get:
       0----1----2----3----4----7----9--> audio
       0----1----2----3----4----> video
     Partial audio frame will be removed in a future cleanup. *)
  assert (G.video_length gen = 5 * frame_size);
  assert (G.audio_length gen = (6 * frame_size) + (frame_size / 2));
  assert (G.length gen = 5 * frame_size);

  (* Add 7---- video *)
  put_video ~pts:7L gen 0 frame_size;
  (* Get:
       0----1----2----3----4----7----9--> audio
       0----1----2----3----4----7----> video *)
  assert (G.video_length gen = 6 * frame_size);
  assert (G.audio_length gen = (6 * frame_size) + (frame_size / 2));
  assert (G.length gen = 6 * frame_size);

  (* Add 9---- video *)
  put_video ~pts:9L gen 0 frame_size;
  (* Get:
       0----1----2----3----4----7----9--> audio
       0----1----2----3----4----7----9----> video *)
  assert (G.video_length gen = 7 * frame_size);
  assert (G.audio_length gen = (6 * frame_size) + (frame_size / 2));
  assert (G.length gen = 6 * frame_size);

  (* Add 10-- audio (partial out-of-sync) *)
  put_audio ~pts:10L gen 0 (frame_size / 2);
  (* Get:
     0----1----2----3----4----7----10--> audio
     0----1----2----3----4----7----9----> video *)
  assert (G.video_length gen = 7 * frame_size);
  assert (G.audio_length gen = (6 * frame_size) + (frame_size / 2));
  assert (G.length gen = 6 * frame_size);

  (* Add 11---- audio *)
  put_audio ~pts:11L gen 0 frame_size;
  (* Get:
     0----1----2----3----4----7----11----> audio
     0----1----2----3----4----7----> video *)
  assert (G.video_length gen = 6 * frame_size);
  assert (G.audio_length gen = 7 * frame_size);
  assert (G.length gen = 6 * frame_size);

  (* Add 11---- video *)
  put_video ~pts:11L gen 0 frame_size;
  (* Get:
     0----1----2----3----4----7----11----> audio
     0----1----2----3----4----7----11----> video *)
  assert (G.video_length gen = 7 * frame_size);
  assert (G.audio_length gen = 7 * frame_size);
  assert (G.length gen = 7 * frame_size)
