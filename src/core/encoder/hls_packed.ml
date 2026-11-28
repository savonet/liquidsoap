let codecs format =
  try Some (Encoder.iso_base_file_media_file_format format)
  with Not_found -> None

let bitrate format = try Some (Encoder.bitrate format) with Not_found -> None

let make ~format (encoder : Encoder.encoder) : Encoder.hls_encoder =
  let time = ref 0 in
  (* Until the output's offset is set, the bytes are held: the first
     segment's tag carries it. *)
  let offset = ref None in
  let held = ref Strings.empty in
  let version = ref 3 in
  let encode frame =
    let bytes = encoder.encode frame in
    time := !time + Frame.position frame;
    bytes
  in
  {
    init =
      (fun ?id3 ?id3_version () ->
        match id3 with
          | Some false -> Error "packed audio requires ID3 (RFC 8216 §3.4)"
          | _ ->
              Option.iter (fun v -> version := v) id3_version;
              Ok `Prepended);
    init_encode =
      (fun ~boundary:_ frame ->
        held := Strings.append !held (encode frame);
        match !offset with
          | None -> Pending
          | Some _ ->
              let bytes = !held in
              held := Strings.empty;
              Init { init = None; pieces = [Bytes bytes] });
    split_encode =
      (fun ~boundary:_ frame ->
        let split = Encoder.Split { time = !time } in
        [split; Bytes (encode frame)]);
    insert_id3 =
      (fun ~time m ->
        let offset = Option.value ~default:0 !offset in
        Some (Hls_id3.make ~version:!version ~time:(time + offset) m));
    initial_delay = (fun () -> Some 0);
    set_offset = (fun o -> offset := Some o);
    hls_stop =
      (fun () ->
        let bytes = Strings.append !held (encoder.stop ()) in
        held := Strings.empty;
        { init = None; pieces = [Bytes bytes] });
    codec_attrs = (fun () -> codecs format);
    independent_segments = (fun () -> true);
    free_split = (fun () -> true);
    hls_bitrate = (fun () -> bitrate format);
    hls_video_size = (fun () -> None);
  }

let () =
  Plug.register Encoder.hls_plug "packed-audio"
    ~doc:"Packed audio with ID3 timestamps." (fun format ->
      match format with
        | Encoder.MP3 _ | Encoder.Shine _
        | Encoder.FdkAacEnc { Fdkaac_format.transmux = `Adts; _ } ->
            Some
              (fun ~pos ~segment_duration:_ name ->
                make ~format
                  ((Encoder.get_factory format)
                     ~pos name Frame.Metadata.Export.empty))
        | _ -> None)
