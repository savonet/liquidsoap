(* Conformance check of spec/encoder-packed-audio.md §7: the example bytes. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let hex s =
  String.concat " "
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let expected =
  "49 44 33 03 00 00 00 00 00 50 54 49 54 32 00 00 00 07 c0 00 01 ff fe 54 00 \
   31 00 50 52 49 56 00 00 00 35 c0 00 63 6f 6d 2e 61 70 70 6c 65 2e 73 74 72 \
   65 61 6d 69 6e 67 2e 74 72 61 6e 73 70 6f 72 74 53 74 72 65 61 6d 54 69 6d \
   65 73 74 61 6d 70 00 00 00 00 00 00 fc af 80"

let () =
  Frame_settings.lazy_config_eval := true;
  let rate = Lazy.Mutexed.force Frame.main_rate in
  let m =
    Frame.Metadata.Export.from_metadata
      (Frame.Metadata.from_list [("title", "T1")])
  in
  let tag = Hls_id3.make ~version:3 ~time:(184 * rate) m in
  if hex tag <> expected then
    Printf.printf "got:      %s\nexpected: %s\n" (hex tag) expected;
  check "spec example bytes" (hex tag = expected);
  let empty = Hls_id3.make ~version:3 ~time:0 Frame.Metadata.Export.empty in
  check "empty metadata still carries the timestamp"
    (String.length empty = 10 + 10 + 53);
  (* 0.0232 s frames: the timestamp is rounded from the tick count. *)
  let ticks = Frame.main_of_seconds 0.0232 in
  let tag =
    Hls_id3.make ~version:3 ~time:(1000 * ticks) Frame.Metadata.Export.empty
  in
  let ts = String.get_int64_be tag (String.length tag - 8) in
  check "timestamp from ticks"
    (ts
    = Int64.of_float
        (Float.round (float (1000 * ticks) *. 90000. /. float rate)));
  Printf.printf "id3: %d checks passed\n" !checks;
  if !checks < 3 then failwith "too few checks ran"
