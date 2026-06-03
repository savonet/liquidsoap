let info file =
  let mf = Mad.openfile file in
  ignore (Mad.decode_frame mf);
  let sample_freq, channels, _ = Mad.get_output_format mf in
  Mad.close mf;
  Printf.printf "Samplefreq:\t%dHz\nChannels\t%d\nDuration:\t%fs\n" sample_freq
    channels (Mad.duration file)

let _ = info Sys.argv.(1)
