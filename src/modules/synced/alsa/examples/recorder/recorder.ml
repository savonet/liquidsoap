open Alsa

let period_size = 4096
let nb_periods = 4
let period_frames = period_size / 4

type mode = Recorder | Player | Loopback

let mode = ref Recorder

let set_params dev =
  let params = Pcm.get_params dev in
  Pcm.set_access dev params Pcm.Access_rw_interleaved;
  Pcm.set_format dev params Pcm.Format_s16_le;
  ignore (Pcm.set_rate_near dev params 44100 Dir_eq);
  Pcm.set_channels dev params 2;
  Pcm.set_buffer_size dev params (period_frames * nb_periods);
  Pcm.set_params dev params

let player () =
  let buf = Bytes.create period_size in
  let dev = Pcm.open_pcm "hw:0,0" [Pcm.Playback] [] in
  set_params dev;
  Pcm.prepare dev;
  while true do
    assert (input stdin buf 0 period_size = period_size);
    try ignore (Pcm.writei dev buf 0 period_frames)
    with Buffer_xrun -> Printf.eprintf "Buffer xrun!\n%!"
  done

let recorder () =
  let buf = Bytes.create period_size in
  let dev = Pcm.open_pcm "hw:0,0" [Pcm.Capture] [] in
  set_params dev;
  Pcm.prepare dev;
  while true do
    let _ = Pcm.readi dev buf 0 period_frames in
    Printf.printf "%s%!" (Bytes.to_string buf)
  done

let loopback () =
  let buf = Bytes.create period_size in
  let dev = Pcm.open_pcm "hw:0,0" [Pcm.Playback; Pcm.Capture] [] in
  set_params dev;
  Pcm.prepare dev;
  while true do
    let _ = Pcm.readi dev buf 0 period_frames in
    ignore (Pcm.wait dev (-1));
    Pcm.prepare dev;
    ignore (Pcm.writei dev buf 0 period_frames)
  done

let () =
  Arg.parse
    [
      ("-p", Arg.Unit (fun () -> mode := Player), "player mode");
      ("-l", Arg.Unit (fun () -> mode := Loopback), "loopback mode");
    ]
    (fun _ -> ())
    "recorder";
  match !mode with
    | Recorder -> recorder ()
    | Player -> player ()
    | Loopback -> loopback ()
