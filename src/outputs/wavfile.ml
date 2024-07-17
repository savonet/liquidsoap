
(** Output in a WAV file. *)

let output name loop =
  (*<section wav output>*)
  let name =
    Dtools.Conf.get_string ~root:name ~default:"/dev/null" "filename" in
  let fd = open_out name in
    (*</section>*)
    output_string fd (Wav.header Mixer.Buffer.format) ;
    loop
      (fun wav ->
         output_string fd (Mixer.Buffer.to_string wav)) ;
    close_out fd

let _ =
  Output.plug#register "wav" output
