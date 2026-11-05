let owner = "com.apple.streaming.transportStreamTimestamp"

(* Rounded from the tick count so that frames that are not a whole number of
   90 kHz units do not drift. *)
let timestamp time =
  let ticks_per_second = Lazy.Mutexed.force Frame.main_rate in
  let ts =
    Int64.div
      (Int64.add
         (Int64.mul (Int64.of_int time) 90000L)
         (Int64.of_int (ticks_per_second / 2)))
      (Int64.of_int ticks_per_second)
  in
  Int64.logand ts 0x1_ffff_ffffL

let make ~version ~time metadata =
  let data = Bytes.create 8 in
  Bytes.set_int64_be data 0 (timestamp time);
  let priv =
    {
      Metadata.ID3v2.id = `PRIV;
      data = `Binary (owner ^ "\000" ^ Bytes.to_string data);
      flags = Metadata.ID3v2.default_flags `PRIV;
    }
  in
  Utils.id3v2_of_metadata ~extra_frames:[priv] ~version
    (Frame.Metadata.Export.to_list metadata)
