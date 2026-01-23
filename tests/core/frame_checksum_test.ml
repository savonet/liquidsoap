open Content

let () =
  Frame_settings.conf_duration#set 0.04;
  Frame_settings.lazy_config_eval := true

(* Test that checksum is deterministic for same content *)
let () =
  let length = Lazy.force Frame.size in
  let ctype =
    Frame_type.content_type
      (Lang.frame_t Lang.unit_t
         (Frame.Fields.make ~audio:(Format_type.audio ()) ()))
  in
  let frame = Frame.create ~length ctype in
  let checksum1 = Frame.checksum frame in
  let checksum2 = Frame.checksum frame in
  assert (checksum1 = checksum2)

(* Test that different content produces different checksums *)
let () =
  let length = Lazy.force Frame.size in
  let ctype =
    Frame_type.content_type
      (Lang.frame_t Lang.unit_t
         (Frame.Fields.make ~audio:(Format_type.audio ()) ()))
  in
  let frame1 = Frame.create ~length ctype in
  let frame2 = Frame.create ~length ctype in
  (* Modify frame2's audio content *)
  let audio = Frame.audio frame2 in
  let audio_data = Content.Audio.get_data audio in
  if Array.length audio_data > 0 && Array.length audio_data.(0) > 0 then
    audio_data.(0).(0) <- 0.5;
  let checksum1 = Frame.checksum frame1 in
  let checksum2 = Frame.checksum frame2 in
  assert (checksum1 <> checksum2)

(* Test content checksum for track marks *)
let () =
  let c1 = Content.make ~length:1000 Content.Track_marks.format in
  let c2 = Content.make ~length:1000 Content.Track_marks.format in
  Track_marks.set_data c1 [100; 200; 300];
  Track_marks.set_data c2 [100; 200; 300];
  let checksum1 = Content.checksum c1 in
  let checksum2 = Content.checksum c2 in
  assert (checksum1 = checksum2);
  (* Different track marks should produce different checksums *)
  Track_marks.set_data c2 [100; 200; 400];
  let checksum3 = Content.checksum c2 in
  assert (checksum1 <> checksum3)

(* Test content checksum for metadata *)
let () =
  let c1 = Content.make ~length:1000 Content.Metadata.format in
  let c2 = Content.make ~length:1000 Content.Metadata.format in
  let m = Frame.Metadata.from_list [("artist", "Test"); ("title", "Song")] in
  Metadata.set_data c1 [(100, m)];
  Metadata.set_data c2 [(100, m)];
  let checksum1 = Content.checksum c1 in
  let checksum2 = Content.checksum c2 in
  assert (checksum1 = checksum2);
  (* Different metadata should produce different checksums *)
  let m2 = Frame.Metadata.from_list [("artist", "Other")] in
  Metadata.set_data c2 [(100, m2)];
  let checksum3 = Content.checksum c2 in
  assert (checksum1 <> checksum3)

(* Test that checksum format is a valid hex string *)
let () =
  let length = Lazy.force Frame.size in
  let ctype =
    Frame_type.content_type
      (Lang.frame_t Lang.unit_t
         (Frame.Fields.make ~audio:(Format_type.audio ()) ()))
  in
  let frame = Frame.create ~length ctype in
  let checksum = Frame.checksum frame in
  (* MD5 hex is 32 characters *)
  assert (String.length checksum = 32);
  (* All characters should be hex digits *)
  String.iter
    (fun c ->
      assert (
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')))
    checksum

let () = print_endline "All frame checksum tests passed!"
