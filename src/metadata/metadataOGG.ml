open MetadataBase
module R = Reader

let parse f : metadata =
  (* Packetized reading *)
  let f, peek =
    (* Current page *)
    let page = ref "" in
    (* Read a page *)
    let fill () =
      if R.read f 4 <> "OggS" then raise Invalid;
      (* stream_structure_version *)
      ignore (R.read f 1);
      (* header_type_flag *)
      ignore (R.read f 1);
      (* absolute granule position *)
      ignore (R.read f 8);
      (* stream serial number *)
      ignore (R.read f 4);
      (* page sequence no *)
      ignore (R.read f 4);
      (* page checksum *)
      ignore (R.read f 4);
      let segments = R.uint8 f in
      let lacing = List.init segments (fun _ -> R.uint8 f) in
      let n = List.fold_left ( + ) 0 lacing in
      page := !page ^ R.read f n
    in
    let ensure len =
      while String.length !page < len do
        fill ()
      done
    in
    let read b off len =
      ensure len;
      Bytes.blit_string !page 0 b off len;
      page := String.sub !page len (String.length !page - len);
      len
    in
    let seek n =
      assert (n >= 0);
      ensure n;
      page := String.sub !page n (String.length !page - n)
    in
    let peek n =
      ensure n;
      let buf = Bytes.create n in
      Bytes.blit_string !page 0 buf 0 n;
      Bytes.unsafe_to_string buf
    in
    ( { R.read; seek; size = (fun () -> None); reset = (fun () -> assert false) },
      peek )
  in
  let comments () =
    let string () =
      let n = R.uint32_le f in
      R.read f n
    in
    let vendor = string () in
    let n = R.uint32_le f in
    let comments = List.init n (fun _ -> string ()) in
    let comments =
      List.filter_map
        (fun c ->
          match String.index_opt c '=' with
            | Some n ->
                Some
                  ( String.sub c 0 n,
                    String.sub c (n + 1) (String.length c - (n + 1)) )
            | None -> None)
        comments
    in
    ("vendor", vendor) :: comments
  in
  if peek 8 = "OpusHead" then (
    (* version *)
    let v = R.uint8 f in
    if v <> 1 then raise Invalid;
    (* output channels *)
    let c = R.uint8 f in
    (* pre-skip *)
    ignore (R.uint16_le f);
    (* input samplerate *)
    ignore (R.uint32_le f);
    (* output gain *)
    ignore (R.uint16_le f);
    (* mapping family *)
    let mapping_family = R.uint8 f in
    if mapping_family <> 0 then (
      (* stream count *)
      ignore (R.uint8 f);
      (* coupled count *)
      ignore (R.uint8 f);
      (* channel mapping *)
      ignore (R.read f c));
    if R.read f 8 <> "OpusTags" then raise Invalid;
    comments ())
  else (
    (* Assume vorbis *)
    let t = R.uint8 f in
    (* Packet type *)
    if R.read f 6 <> "vorbis" then raise Invalid;
    (* identification header *)
    assert (t = 1);
    R.drop f (4 + 1 + 4 + 4 + 4 + 4 + 2);
    (* comment header *)
    assert (R.uint8 f = 3);
    if R.read f 6 <> "vorbis" then raise Invalid;
    comments ())

let parse_file = R.with_file parse
