let () = Printexc.record_backtrace true

let output_int chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff));
  output_char chan (char_of_int ((n lsr 16) land 0xff));
  output_char chan (char_of_int ((n lsr 24) land 0xff))

let output_short chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff))

let progress_bar =
  let spin = ref 0 in
  fun title pos tot ->
    let nbeq = 40 in
    let n = min (100. *. float_of_int pos /. float_of_int tot) 100. in
    Printf.printf "\r%s " title;
    if tot > 0 then begin
      Printf.printf "%6.2f%% [" n;
      let e = int_of_float (n /. 100. *. float_of_int nbeq) in
      for _ = 1 to e do
        Printf.printf "="
      done;
      if e != nbeq then Printf.printf ">";
      for _ = e + 2 to nbeq do
        Printf.printf " "
      done;
      Printf.printf "] "
    end;
    incr spin;
    if !spin > 4 then spin := 1;
    Printf.printf "%c%!"
      (if tot > 0 && n = 100. then ' '
       else (
         match !spin with
           | 1 -> '|'
           | 2 -> '/'
           | 3 -> '-'
           | 4 -> '\\'
           | _ -> failwith "this did not happen"))

let infile = ref "input.flac"
let outfile = ref "output.raw"
let ogg = ref false

let () =
  Arg.parse
    [
      ("-o", Arg.Set_string outfile, "Output file");
      ("-i", Arg.Set_string infile, "Input file");
      ("-ogg", Arg.Bool (fun x -> ogg := x), "Ogg/flac file");
    ]
    ignore "decode [options]"

let process () =
  let fd =
    Printf.printf "Opening input file %S\n%!" !infile;
    Unix.openfile !infile [Unix.O_RDONLY] 0o640
  in
  let oc =
    Printf.printf "Opening output file %S\n%!" !outfile;
    open_out !outfile
  in
  let ret = Buffer.create 1024 in
  let write x = Buffer.add_string ret (Flac.Decoder.to_s16le x) in
  let get () =
    let ans = Buffer.contents ret in
    Buffer.reset ret;
    ans
  in
  let process, info, comments =
    if not !ogg then (
      let h = Flac.Decoder.File.create_from_fd ~write fd in
      let process () =
        Flac.Decoder.process h.Flac.Decoder.File.dec;
        Flac.Decoder.state h.Flac.Decoder.File.dec
      in
      (process, h.Flac.Decoder.File.info, h.Flac.Decoder.File.comments))
    else (
      let sync = Ogg.Sync.create (Unix.read fd) in
      let test_flac () =
        (* Get First page *)
        let page = Ogg.Sync.read sync in
        (* Check whether this is a b_o_s *)
        if not (Ogg.Page.bos page) then raise Flac.Decoder.Not_flac;
        (* Create a stream with this ID *)
        let serial = Ogg.Page.serialno page in
        Printf.printf "Testing stream %nx\n" serial;
        let os = Ogg.Stream.create ~serial () in
        Ogg.Stream.put_page os page;
        let packet = Ogg.Stream.peek_packet os in
        (* Test header. Do not catch anything, first page should be sufficient *)
        if not (Flac_ogg.Decoder.check_packet packet) then raise Not_found;
        Printf.printf "Got a flac stream !\n";
        let fill () =
          let page = Ogg.Sync.read sync in
          if Ogg.Page.serialno page = serial then Ogg.Stream.put_page os page
        in
        let dec, info, meta = Flac_ogg.Decoder.create ~fill ~write os in
        let rec process () =
          try
            Flac.Decoder.process dec;
            Flac.Decoder.state dec
          with
            | Ogg.End_of_stream -> `End_of_stream
            | Ogg.Not_enough_data -> (
                try
                  fill ();
                  process ()
                with Ogg.End_of_stream | Ogg.Not_enough_data -> `End_of_stream)
        in
        (process, info, meta)
      in
      (* Now find a flac stream *)
      let rec init () =
        try test_flac () with
          | Not_found ->
              Printf.printf "This stream was not flac..\n";
              init ()
          | Flac.Decoder.Not_flac ->
              Printf.printf "No flac stream was found..\n%!";
              raise Flac.Decoder.Not_flac
      in
      init ())
  in
  Printf.printf "Stream info:\n";
  Printf.printf "sample rate: %i\n" info.Flac.Decoder.sample_rate;
  Printf.printf "bits per sample: %i\n" info.Flac.Decoder.bits_per_sample;
  Printf.printf "channels: %i\n" info.Flac.Decoder.channels;
  Printf.printf "total samples: %s\n"
    (Int64.to_string info.Flac.Decoder.total_samples);
  Printf.printf "md5sum: ";
  String.iter
    (fun c -> Printf.printf "%x" (int_of_char c))
    info.Flac.Decoder.md5sum;
  Printf.printf "\n";
  if info.Flac.Decoder.bits_per_sample <> 16 then
    failwith "Unsupported bits per sample.";
  let srate = info.Flac.Decoder.sample_rate in
  let chans = info.Flac.Decoder.channels in
  let datalen = Int64.to_int info.Flac.Decoder.total_samples * chans * 2 in
  let () =
    match comments with
      | None -> Printf.printf "No comment found..\n"
      | Some (vendor, comments) ->
          Printf.printf "Metadata:\n";
          List.iter (fun (x, y) -> Printf.printf "%s: %s\n" x y) comments;
          Printf.printf "VENDOR: %s\n" vendor
  in
  output_string oc "RIFF";
  output_int oc (4 + 24 + 8 + datalen);
  output_string oc "WAVE";
  output_string oc "fmt ";
  output_int oc 16;
  output_short oc 1;
  (* WAVE_FORMAT_PCM *)
  output_short oc chans;
  (* channels *)
  output_int oc srate;
  (* freq *)
  output_int oc (srate * chans * 2);
  (* bytes / s *)
  output_short oc (chans * 2);
  (* block alignment *)
  output_short oc 16;
  (* bits per sample *)
  output_string oc "data";
  output_int oc datalen;
  let pos = ref 0 in
  let rec decode () =
    let state = process () in
    let ret = get () in
    pos := !pos + String.length ret;
    progress_bar "Decoding FLAC file:" !pos datalen;
    output_string oc ret;
    flush oc;
    match state with `End_of_stream -> Printf.printf "\n" | _ -> decode ()
  in
  decode ();
  Printf.printf "\n";
  close_out oc;
  Unix.close fd

let () =
  process ();
  (* We have global root values
   * so we need to do two full major.. *)
  Gc.full_major ();
  Gc.full_major ()
