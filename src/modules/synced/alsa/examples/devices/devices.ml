open Alsa

let samplerate = 44100
let channels = 2

let () =
  Printf.printf "Using ALSA %s.\n%!" (Alsa.get_version ());
  List.iter
    (fun (name, desc, io) ->
      let io =
        match io with
          | `Input -> "input"
          | `Output -> "output"
          | `Both -> "both"
      in
      Printf.printf "%s\n%s\n%s\n\n%!" name io desc)
    (device_name_hints ())
