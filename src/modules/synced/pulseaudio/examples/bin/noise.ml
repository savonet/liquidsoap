open Pulseaudio

let () =
  let ss =
    {
      sample_format = Sample_format_float32le;
      sample_rate = 44100;
      sample_chans = 2;
    }
  in
  let simple =
    try
      Simple.create ~client_name:"Test" ~dir:Dir_playback ~stream_name:"Noise"
        ~sample:ss ()
    with Error n ->
      Printf.eprintf "Error: %s\n%!" (string_of_error n);
      exit (-1)
  in
  let buflen = 10240 in
  let buf =
    Array.init 2 (fun _ -> Array.init buflen (fun _ -> Random.float 2. -. 1.))
  in
  while true do
    Simple.write simple buf 0 buflen
  done;
  Simple.free simple
