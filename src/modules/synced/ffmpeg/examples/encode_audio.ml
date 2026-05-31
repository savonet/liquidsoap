open Avcodec
module Resampler = Swresample.Make (Swresample.FloatArray) (Swresample.Frame)

let ( %> ) f g x = g (f x)
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
  let time_base = { Avutil.num = 1; den = sample_rate } in
  let encoder =
    Audio.create_encoder ~channel_layout:Avutil.Channel_layout.stereo ~time_base
      ~sample_format:out_sample_format ~sample_rate codec
  in

  let () =
    let p = Avcodec.params encoder in
    Printf.printf "Codec ID: %s\n%!"
      Avcodec.Audio.(string_of_id (get_params_id p))
  in

  let frame_size =
    if List.mem `Variable_frame_size (capabilities codec) then 512
    else Audio.frame_size encoder
  in

  let out_sample_format = Audio.find_best_sample_format codec `Dbl in

  let rsp =
    Resampler.create Avutil.Channel_layout.mono sample_rate
      Avutil.Channel_layout.stereo ~out_sample_format sample_rate
  in

  let c = 2. *. pi *. 440. /. float_of_int sample_rate in

  let out_file = open_out_bin Sys.argv.(1) in

  for i = 0 to 2000 do
    Array.init (2 * frame_size) (fun t ->
        sin (float_of_int (t + (i * frame_size)) *. c))
    |> Resampler.convert ~offset:10 ~length:frame_size rsp
    |> Avcodec.encode encoder (Packet.to_bytes %> output_bytes out_file)
  done;

  Avcodec.flush_encoder encoder (Packet.to_bytes %> output_bytes out_file);

  close_out out_file;

  Gc.full_major ();
  Gc.full_major ()
