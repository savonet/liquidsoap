let codecs format =
  try Some (Encoder.iso_base_file_media_file_format format)
  with Not_found -> None

let bitrate format = try Some (Encoder.bitrate format) with Not_found -> None

let make ~format (encoder : Encoder.encoder) : Encoder.hls_encoder =
  let time = ref 0 in
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
        Init { init = None; pieces = [Bytes (encode frame)] });
    split_encode =
      (fun ~boundary:_ ~force frame ->
        let split = Encoder.Split { time = !time; forced = force } in
        [split; Bytes (encode frame)]);
    insert_id3 = (fun ~time m -> Some (Hls_id3.make ~version:!version ~time m));
    hls_header = (fun () -> Strings.empty);
    hls_stop = (fun () -> [Bytes (encoder.stop ())]);
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
              (fun ~pos name ->
                make ~format
                  ((Encoder.get_factory format)
                     ~pos name Frame.Metadata.Export.empty))
        | _ -> None)
