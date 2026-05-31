open Avcodec
module Resampler = Swresample.Make (Swresample.FloatArray) (Swresample.Frame)

let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: %s <output file> <codec name>\n" Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let pi = 4.0 *. atan 1.0 in
  let sample_rate = 44100 in

  let codec = Audio.find_encoder_by_name Sys.argv.(2) in
  let out_sample_format = Audio.find_best_sample_format codec `Dbl in

  let out_sample_rate = if Sys.argv.(2) = "flac" then 22050 else 44100 in

  let rsp =
    Resampler.create Avutil.Channel_layout.mono sample_rate
      Avutil.Channel_layout.stereo ~out_sample_format out_sample_rate
  in

  let c = 2. *. pi *. 440. /. float_of_int sample_rate in

  let filename = Sys.argv.(1) in
  let format =
    match Av.Format.guess_output_format ~filename () with
      | None -> failwith "No format for filename!"
      | Some f -> f
  in
  let fd = Unix.(openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o644) in
  let write = Unix.write fd in
  let seek = Unix.lseek fd in

  let output_opt = Hashtbl.create 2 in
  Hashtbl.add output_opt "packetsize" (`Int 4096);
  Hashtbl.add output_opt "foo" (`String "bla");

  let output = Av.open_output_stream ~opts:output_opt ~seek write format in

  let pts = ref 0L in
  let time_base = { Avutil.num = 1; den = out_sample_rate } in

  let opts = Hashtbl.create 2 in
  Hashtbl.add opts "lpc_type" (`String "none");
  Hashtbl.add opts "foo" (`String "bla");

  let stream =
    Av.new_audio_stream ~channel_layout:Avutil.Channel_layout.stereo ~time_base
      ~sample_format:out_sample_format ~sample_rate:out_sample_rate ~codec ~opts
      output
  in

  let out_frame_size =
    if List.mem `Variable_frame_size (capabilities codec) then 512
    else Av.get_frame_size stream
  in

  let audio_filter =
    let in_params =
      {
        Avfilter.Utils.sample_rate = out_sample_rate;
        channel_layout = Avutil.Channel_layout.stereo;
        sample_format = out_sample_format;
      }
    in
    Avfilter.Utils.init_audio_converter ~in_params ~in_time_base:time_base
      ~out_frame_size ()
  in

  assert (Hashtbl.mem opts "foo");
  if Sys.argv.(2) = "flac" then assert (not (Hashtbl.mem opts "lpc_type"))
  else assert (Hashtbl.mem opts "lpc_type");

  assert (Hashtbl.mem output_opt "foo");
  assert (not (Hashtbl.mem output_opt "packetsize"));

  let on_frame frame =
    Avutil.Frame.set_pts frame (Some !pts);
    pts := Int64.add !pts (Int64.of_int (Avutil.Audio.frame_nb_samples frame));
    Av.write_frame stream frame
  in

  for i = 0 to 2000 do
    Array.init out_frame_size (fun t ->
        sin (float_of_int (t + (i * out_frame_size)) *. c))
    |> Resampler.convert rsp
    |> fun frame ->
    Avfilter.Utils.convert_audio audio_filter on_frame (`Frame frame)
  done;

  Avfilter.Utils.convert_audio audio_filter on_frame `Flush;
  Av.close output;
  Unix.close fd;

  Gc.full_major ();
  Gc.full_major ()
