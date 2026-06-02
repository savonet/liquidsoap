open Alsa

let samplerate = 44100
let channels = 2

let () =
  Printf.printf "Using ALSA %s.\n%!" (Alsa.get_version ());
  let dev = Pcm.open_pcm "default" [Pcm.Playback] [] in
  let params = Pcm.get_params dev in
  Pcm.set_access dev params Pcm.Access_rw_interleaved;
  Pcm.set_format dev params Pcm.Format_float;
  let samplerate = Pcm.set_rate_near dev params samplerate Dir_eq in
  Pcm.set_channels dev params channels;
  Pcm.set_params dev params;
  let buffer_size = Pcm.get_buffer_size params in
  let period_size = Pcm.get_period_size params in
  Printf.printf "samplerate: %d, buffer size: %d, period size: %d\n%!"
    samplerate buffer_size period_size;
  Pcm.prepare dev;
  let buf = Array.init channels (fun _ -> Array.make period_size 0.) in
  let t = ref 0. in
  while true do
    Printf.printf "time: %f\r%!" !t;
    for i = 0 to period_size - 1 do
      let x = sin (2. *. Float.pi *. !t *. 440.) in
      t := !t +. (1. /. float samplerate);
      for c = 0 to channels - 1 do
        buf.(c).(i) <- x
      done
    done;
    ignore (Pcm.writei_floatn dev buf 0 period_size)
  done
