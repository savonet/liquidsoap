open MetadataBase
module R = Reader

let parse f : metadata =
  (* Packetized reading *)
  let f =
    (* Current page *)
    let page = ref "" in
    (* Read a page *)
    let fill () =
      if R.read f 4 <> "OggS" then raise Invalid;
      ignore (R.read f 1);
      (* stream_structure_version *)
      ignore (R.read f 1);
      (* header_type_flag *)
      ignore (R.read f 8);
      (* absolute granule position *)
      ignore (R.read f 4);
      (* stream serial number *)
      ignore (R.read f 4);
      (* page sequence no *)
      ignore (R.read f 4);
      (* page checksum *)
      let segments = R.uint8 f in
      let lacing = List.init segments (fun _ -> R.uint8 f) in
      let n = List.fold_left ( + ) 0 lacing in
      page := !page ^ R.read f n
    in
    let read b off len =
      while String.length !page < len do
        fill ()
      done;
      Bytes.blit_string !page 0 b off len;
      page := String.sub !page len (String.length !page - len);
      len
    in
    {
      R.read;
      seek = (fun n -> if n <> 0 then assert false);
      size = (fun () -> None);
      reset = (fun () -> assert false);
    }
  in
  if R.read f 8 = "OpusHead" then (
    let v = R.uint8 f in
    (* version *)
    if v <> 1 then raise Invalid;
    let c = R.uint8 f in
    (* output channels *)
    ignore (R.uint16_le f);
    (* pre-skip *)
    ignore (R.uint32_le f);
    (* input samplerate *)
    ignore (R.uint16_le f);
    (* output gain *)
    let mapping_family = R.uint8 f in
    (* mapping family *)
    if mapping_family <> 0 then (
      ignore (R.uint8 f);
      (* stream count *)
      ignore (R.uint8 f);
      (* coupled count *)
      ignore (R.read f c) (* channel mapping *));
    if R.read f 8 <> "OpusTags" then raise Invalid;
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
    ("vendor", vendor) :: comments)
  else raise Invalid

let parse_file = R.with_file parse
