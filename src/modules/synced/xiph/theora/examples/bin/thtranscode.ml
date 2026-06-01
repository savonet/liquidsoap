(* Shamelessly inspired of http://theora.org/doc/libtheora-1.0beta1/ *)

exception No_theora

open Theora

let infile = ref "input.ogg"
let outfile = ref "output.ogg"
let debug = ref false
let quality = ref 40

let () =
  Arg.parse
    [
      ("-d", Arg.Set debug, "Show debugging messages");
      ("-o", Arg.Set_string outfile, "Output file");
      ("-q", Arg.Set_int quality, "Quality of the compression");
      ("-i", Arg.Set_string infile, "Input file");
    ]
    ignore "thranscode [options]"

let in_init () =
  let sync, fd = Ogg.Sync.create_from_file !infile in
  let rec fill os =
    let page = Ogg.Sync.read sync in
    try
      (* We drop pages which are not for us.. *)
      if Ogg.Page.serialno page = Ogg.Stream.serialno os then
        Ogg.Stream.put_page os page
    with Ogg.Bad_data -> fill os
    (* Do not care about page that are not for us.. *)
  in
  (* Test whether the stream is theora *)
  let test_theora () =
    (* Get First page *)
    let page = Ogg.Sync.read sync in
    (* Check whether this is a b_o_s *)
    if not (Ogg.Page.bos page) then raise No_theora;
    (* Create a stream with this ID *)
    let serial = Ogg.Page.serialno page in
    Printf.printf "Testing stream %nx\n" serial;
    let os = Ogg.Stream.create ~serial () in
    Ogg.Stream.put_page os page;
    let packet = Ogg.Stream.get_packet os in
    (* Test header. Do not catch anything, first page should be sufficient *)
    if not (Decoder.check packet) then raise Not_found;
    Printf.printf "Got a theora stream !\n";
    let dec = Decoder.create () in
    (* Decode headers *)
    let rec f packet =
      try Decoder.headerin dec packet
      with Ogg.Not_enough_data ->
        let rec g () =
          try
            let packet = Ogg.Stream.get_packet os in
            f packet
          with Ogg.Not_enough_data ->
            fill os;
            g ()
        in
        g ()
    in
    let dec, info, vendor, comments = f packet in
    (serial, os, dec, info, vendor, comments)
  in
  (* Now find a theora stream *)
  let rec init () =
    try test_theora () with
      | Not_found ->
          Printf.printf "This stream was not theora..\n";
          init ()
      | No_theora ->
          Printf.printf "No theora stream was found..\n%!";
          raise No_theora
  in
  let serial, os, t, info, vendor, comments = init () in
  Printf.printf "Ogg logical stream %nx is Theora %dx%d %.02f fps video\n"
    serial info.frame_width info.frame_height
    (float_of_int info.fps_numerator /. float_of_int info.fps_denominator);
  Printf.printf "Encoded frame content is %dx%d with %dx%d offset\n"
    info.picture_width info.picture_height info.picture_x info.picture_y;
  Printf.printf "YUV4MPEG2 W%d H%d F%d:%d I%c A%d:%d\n" info.frame_width
    info.frame_height info.fps_numerator info.fps_denominator 'p'
    info.aspect_numerator info.aspect_denominator;
  Printf.printf "Vendor: %s\n" vendor;
  List.iter (fun (x, y) -> Printf.printf "%s: %s\n" x y) comments;
  flush_all ();
  (t, os, fill, info, fd)

let out_init info =
  let oc = open_out !outfile in
  let out s =
    output_string oc s;
    flush oc
  in
  let os = Ogg.Stream.create () in
  let settings =
    {
      Encoder.keyframe_frequency = None;
      vp3_compatible = None;
      soft_target = None;
      buffer_delay = None;
      speed = None;
    }
  in
  let comments = [("artitst", "test artist"); ("title", "test title")] in
  let t = Encoder.create info settings comments in
  let s_o_p (h, b) = h ^ b in
  Encoder.encode_header t os;
  out (s_o_p (Ogg.Stream.flush_page os));
  (t, os, out)

let () =
  let dec, is, fill, info, fd = in_init () in
  let info = { info with target_bitrate = 0; quality = !quality } in
  let enc, os, out = out_init info in
  let latest_yuv = ref None in
  let rec generator () =
    try
      let yuv = Decoder.get_yuv dec is in
      latest_yuv := Some yuv;
      yuv
    with
      | Ogg.Not_enough_data when not (Ogg.Stream.eos is) ->
          fill is;
          generator ()
      | Duplicate_frame -> (
          (* Got a duplicate frame, sending previous one ! *)
            match !latest_yuv with
            | Some x -> x
            | None -> raise Internal_error)
  in
  let s_o_p (h, b) = h ^ b in
  Printf.printf "Starting transcoding loop !\n%!";
  begin try
    while true do
      let op = Encoder.encode_page enc os generator in
      let op = s_o_p op in
      out op
    done
  with Ogg.Not_enough_data -> ()
  end;
  List.iter (fun p -> out (s_o_p p)) (Ogg.Stream.terminate os);
  Unix.close fd;
  Gc.full_major ()
