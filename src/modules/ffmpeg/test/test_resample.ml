module R = Swresample.Make (Swresample.FloatArray) (Swresample.S32Bytes)
module R0 = Swresample.Make (Swresample.FloatArray) (Swresample.S16Frame)
module R1 = Swresample.Make (Swresample.Frame) (Swresample.U8BigArray)
module R2 = Swresample.Make (Swresample.U8BigArray) (Swresample.DblPlanarFrame)

module R3 =
  Swresample.Make (Swresample.DblPlanarFrame) (Swresample.S32PlanarBigArray)

module R4 =
  Swresample.Make (Swresample.S32PlanarBigArray) (Swresample.FltPlanarBigArray)

module R5 =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.PlanarFloatArray)

module R6 = Swresample.Make (Swresample.PlanarFloatArray) (Swresample.S32Frame)
module R7 = Swresample.Make (Swresample.S32Frame) (Swresample.FloatArray)
module R8 = Swresample.Make (Swresample.FloatArray) (Swresample.S32BigArray)
module R9 = Swresample.Make (Swresample.S32BigArray) (Swresample.S32Bytes)
module R10 = Swresample.Make (Swresample.S32Bytes) (Swresample.S32Bytes)
module ConverterInput = Swresample.Make (Swresample.Frame)
module Converter = ConverterInput (Swresample.PlanarFloatArray)

let write_bytes = output_bytes
let foi = float_of_int
let pi = 4.0 *. atan 1.0
let rate = 44100
let frate = float_of_int rate

let () =
  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let dst1 = open_out_bin "test_swresample_out1.raw" in
  let r =
    R.create Avutil.Channel_layout.mono rate Avutil.Channel_layout.stereo 44100
  in

  let dst2 = open_out_bin "test_swresample_out2.raw" in

  let r0 =
    R0.create Avutil.Channel_layout.mono rate
      Avutil.Channel_layout.five_point_one 96000
  in
  let r1 =
    R1.create Avutil.Channel_layout.five_point_one ~in_sample_format:`S16 96000
      Avutil.Channel_layout.stereo 16000
  in
  let r2 =
    R2.create Avutil.Channel_layout.stereo 16000 Avutil.Channel_layout.stereo
      44100
  in
  let r3 =
    R3.create Avutil.Channel_layout.stereo 44100 Avutil.Channel_layout.stereo
      48000
  in
  let r4 =
    R4.create Avutil.Channel_layout.stereo 48000
      Avutil.Channel_layout.(find "downmix")
      31000
  in
  let r5 =
    R5.create
      Avutil.Channel_layout.(find "downmix")
      31000 Avutil.Channel_layout.stereo 73347
  in
  let r6 =
    R6.create Avutil.Channel_layout.stereo 73347 Avutil.Channel_layout.stereo
      44100
  in
  let r7 =
    R7.create Avutil.Channel_layout.stereo 44100 Avutil.Channel_layout.stereo
      48000
  in
  let r8 =
    R8.create Avutil.Channel_layout.stereo 48000 Avutil.Channel_layout.stereo
      96000
  in
  let r9 =
    R9.create Avutil.Channel_layout.stereo 96000 Avutil.Channel_layout.stereo
      44100
  in
  let r10 =
    R10.create Avutil.Channel_layout.stereo 44100 Avutil.Channel_layout.mono
      44100
  in

  for note = 0 to 95 do
    let freq = 22.5 *. (2. ** (foi note /. 12.)) in
    let len = int_of_float (frate /. freq *. floor (freq /. 4.)) in
    let c = 2. *. pi *. freq /. frate in
    let src = Array.init len (fun t -> sin (foi t *. c)) in

    src |> R.convert r |> write_bytes dst1;

    src |> R0.convert r0 |> R1.convert r1 |> R2.convert r2 |> R3.convert r3
    |> R4.convert r4 |> R5.convert r5 |> R6.convert r6 |> R7.convert r7
    |> R8.convert r8 |> R9.convert r9 |> R10.convert r10 |> write_bytes dst2
  done;

  close_out dst1;
  close_out dst2;

  let output_planar_float_to_s16le audio_output_file planes =
    let nb_chan = Array.length planes in
    let bytes = Bytes.create 2 in
    let cx = float_of_int 0x7FFF in

    for i = 0 to Array.length planes.(0) - 1 do
      for c = 0 to nb_chan - 1 do
        let v = int_of_float (planes.(c).(i) *. cx) in

        Bytes.set bytes 0 (char_of_int (v land 0xFF));
        Bytes.set bytes 1 (char_of_int ((v lsr 8) land 0xFF));
        write_bytes audio_output_file bytes
      done
    done
  in

  Sys.argv |> Array.to_list |> List.tl
  |> List.iter (fun url ->
      try
        let src = Av.open_input url in
        let idx, is, ic = src |> Av.find_best_audio_stream in
        let rsp = Converter.from_codec ic Avutil.Channel_layout.stereo 44100 in

        let p = try String.rindex url '/' + 1 with Not_found -> 0 in
        let audio_output_filename =
          String.(
            sub url p (length url - p) ^ "." ^ string_of_int idx ^ ".s16le.raw")
        in
        let audio_output_file = open_out_bin audio_output_filename in

        print_endline ("Convert " ^ url ^ " to " ^ audio_output_filename);
        let rec f () =
          match Av.read_input ~audio_frame:[is] src with
            | `Audio_frame (i, frame) when i = idx ->
                Converter.convert rsp frame
                |> output_planar_float_to_s16le audio_output_file;
                f ()
            | exception Avutil.Error `Eof -> ()
            | _ -> f ()
        in
        f ();

        Av.get_input is |> Av.close;
        close_out audio_output_file
      with _ -> print_endline ("No audio stream in " ^ url));

  Gc.full_major ();
  Gc.full_major ()
