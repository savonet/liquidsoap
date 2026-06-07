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
    Simple.create ~client_name:"Test" ~dir:Dir_record ~stream_name:"Noise"
      ~sample:ss ()
  in
  let buflen = 10240 in
  let buf =
    Array.init 2 (fun _ -> Array.init buflen (fun _ -> Random.float 2. -. 1.))
  in
  while true do
    Simple.read simple buf 0 buflen;
    Printf.printf "Read %d samples.\n%!" buflen
  done;
  Simple.free simple
