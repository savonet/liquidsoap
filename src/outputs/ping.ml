
(** Output to /dev/null *)

let output name loop = loop (fun _ -> Dtools.Log.log ~label:name 4 "ping")

let () =
  Output.plug#register "ping" output
