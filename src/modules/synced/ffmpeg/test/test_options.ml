(* Every entry point must report the same unused keys: after the call, [opts]
   holds exactly what ffmpeg ignored.

   The large case is a round-trip stress test and proves nothing about
   rooting, since a small result never triggers a minor collection (4096-word
   floor) and a large one lands in the major heap, so both regimes pass with
   the root in ocaml_avutil_unused_options deliberately removed. *)

let bogus = ["definitely_not_an_option"; "also_bogus"]

let mk_opts extra =
  let opts = Hashtbl.create 8 in
  List.iter (fun k -> Hashtbl.replace opts k (`String "1")) bogus;
  List.iter (fun (k, v) -> Hashtbl.replace opts k v) extra;
  opts

let check_unused what opts =
  let left = Hashtbl.fold (fun k _ acc -> k :: acc) opts [] in
  Test_assert.checkf
    (List.sort compare left = List.sort compare bogus)
    "%s reports unused options %s" what
    (String.concat "," (List.sort compare left))

let () =
  let url = Sys.argv.(1) in

  (* Demuxer: probesize is real, the other two are not. *)
  let opts = mk_opts [("probesize", `Int 5000000)] in
  let input = Av.open_input ~opts url in
  check_unused "Av.open_input" opts;

  (* Muxer. *)
  let opts = mk_opts [] in
  let output = Av.open_output ~opts "test_options_out.mkv" in
  check_unused "Av.open_output" opts;

  (* Encoder: b is a real AVCodecContext option. *)
  let opts = mk_opts [("b", `Int 128000)] in
  let encoder =
    Avcodec.Audio.create_encoder ~opts
      ~channel_layout:Avutil.Channel_layout.stereo ~sample_rate:44100
      ~sample_format:`Fltp
      ~time_base:{ Avutil.num = 1; den = 44100 }
      (Avcodec.Audio.find_encoder `Aac)
  in
  check_unused "Avcodec.Audio.create_encoder" opts;
  ignore encoder;

  (* Enough unused keys that the reply collects while its tuple is live. *)
  let n = 20000 in
  let opts = Hashtbl.create n in
  for i = 0 to n - 1 do
    Hashtbl.replace opts (Printf.sprintf "bogus_option_%06d" i) (`String "1")
  done;
  let input2 = Av.open_input ~opts url in
  let left = Hashtbl.length opts in
  Test_assert.checkf (left = n) "%d of %d unused keys survive the reply" left n;
  let corrupt = ref 0 and sample = ref "" in
  Hashtbl.iter
    (fun k _ ->
      let ok =
        String.length k = 19
        && String.sub k 0 13 = "bogus_option_"
        && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub k 13 6)
      in
      if not ok then begin
        incr corrupt;
        sample := k
      end)
    opts;
  Test_assert.checkf (!corrupt = 0) "%d returned keys corrupted (e.g. %S)"
    !corrupt !sample;

  Av.close input;
  Av.close input2;
  Av.close output;
  Gc.full_major ();
  Test_assert.finish ()
