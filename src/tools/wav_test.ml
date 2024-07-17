
let file =
  try Sys.argv.(1) with
    | _ ->
        Printf.printf "Usage: wav_test file.wav\n" ;
        exit (-1)

let fd = Wav.fopen file
let format = Wav.format fd
let abg = Mixer.Generator.create ()
let ab = Mixer.Buffer.create ()
let buflen = Mixer.Buffer.size
let buf = String.make buflen 'x'
let running = ref true

let _ =
  Printf.fprintf stderr "Info:\n%s\n" (Wav.info fd) ;
  print_string (Wav.header Mixer.Buffer.format) ;
  while !running do
    begin
      try
        while Mixer.Generator.should_be_feeded abg do
          let l = Wav.sample fd buf 0 buflen in
            Mixer.Generator.feed abg format (String.sub buf 0 l)
        done
      with
        | End_of_file -> running := false
    end ;
    Mixer.Buffer.free ab ;
    Mixer.Buffer.fill ab abg ;
    print_string (Mixer.Buffer.to_string ab) ;
  done
