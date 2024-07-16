(* Test a stream decoder. ffmpeg/audio only for now. *)

let () =
  if Array.length Sys.argv < 3 then (
    Printf.printf "Usage: stream_decoder_test <in file> <out file>\n%!";
    exit 1);
  Frame_settings.lazy_config_eval := true;
  Dtools.Log.conf_stdout#set true;
  Dtools.Log.conf_file#set false;
  Dtools.Log.conf_level#set 5;
  Dtools.Init.exec Dtools.Log.start;
  let in_file = Sys.argv.(1) in
  let in_fd = open_in_bin in_file in
  let read = input in_fd in
  let out_file = Sys.argv.(2) in
  let out_fd = open_out_bin out_file in
  let write = Strings.iter (output_substring out_fd) in
  let format = "application/ffmpeg" in
  let ctype =
    Frame_type.content_type
      (Lang.frame_t Lang.unit_t
         (Frame.Fields.make ~audio:(Format_type.audio ()) ()))
  in
  let create_decoder = Option.get (Decoder.get_stream_decoder ~ctype format) in
  let decoder =
    create_decoder { Decoder.read; tell = None; length = None; lseek = None }
  in
  let log = Printf.printf "Generator log: %s" in
  let generator = Generator.create ~log ctype in
  let buffer = Decoder.mk_buffer ~ctype generator in
  let mp3_format = Lang_mp3.mp3_base_defaults () in
  let create_encoder = Encoder.get_factory (Encoder.MP3 mp3_format) in
  let encoder =
    create_encoder ~pos:None "test stream" Frame.Metadata.Export.empty
  in
  write (encoder.Encoder.header ());
  let size = Lazy.force Frame.size in
  try
    while true do
      try
        while Generator.length generator < Lazy.force Frame.size do
          decoder.Decoder.decode buffer
        done;
        let frame = Generator.slice generator size in
        write (encoder.Encoder.encode frame 0 (Frame.position frame))
      with Avutil.Error `Invalid_data -> ()
    done
  with Ffmpeg_decoder.End_of_file ->
    write (encoder.Encoder.stop ());
    close_out out_fd;
    close_in in_fd
