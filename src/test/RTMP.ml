(* We are following
   - https://www.adobe.com/devnet/rtmp.html
   - https://www.adobe.com/content/dam/acom/en/devnet/pdf/amf0-file-format-specification.pdf
   - https://www.adobe.com/content/dam/acom/en/devnet/pdf/amf-file-format-spec.pdf
*)

(* A few general remarks:
   - all integers a big-endian
   - timestamps are in miliseconds
*)

(** I/O helper functions. *)

let write f s =
  let n = Bytes.length s in
  assert (Unix.write f s 0 n = n)

let write_byte f n =
  assert (0 <= n && n <= 0xff);
  let s = Bytes.create 1 in
  Bytes.set s 0 (char_of_int n);
  write f s

let write_short f n =
  assert (0 <= n && n <= 0xffff);
  let s = Bytes.create 2 in
  Bytes.set s 0 (char_of_int ((n lsr 8) land 0xff));
  Bytes.set s 1 (char_of_int (n land 0xff));
  write f s

let write_int24 f n =
  let s = Bytes.create 3 in
  for i = 0 to 2 do
    Bytes.set s (2 - i) (char_of_int ((n lsr (8 * i)) land 0xff))
  done;
  write f s

let write_int32 f n =
  let s = Bytes.create 4 in
  for i = 0 to 3 do
    Bytes.set s (3 - i)
      (char_of_int
         (Int32.to_int
            (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n (8 * i)))))
  done;
  write f s

let write_int32_le f n =
  let s = Bytes.create 4 in
  for i = 0 to 3 do
    Bytes.set s i
      (char_of_int
         (Int32.to_int
            (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n (8 * i)))))
  done;
  write f s

let read f n =
  let s = Bytes.create n in
  let r = ref 0 in
  while !r < n do
    let r' = Unix.read f s !r (n - !r) in
    assert (r' > 0);
    r := !r + r'
  done;
  s

let read_string f n = Bytes.unsafe_to_string (read f n)
let read_byte f = int_of_char (read_string f 1).[0]

let read_short f =
  let n = read_byte f in
  (n lsl 8) + read_byte f

let read_int24 f =
  let n = read_byte f in
  let n = (n lsl 8) + read_byte f in
  (n lsl 8) + read_byte f

let read_int32 f =
  let s = read_string f 4 in
  let ans = ref Int32.zero in
  for i = 0 to 3 do
    let n = int_of_char s.[3 - i] in
    let n = Int32.of_int n in
    let n = Int32.shift_left n (i * 8) in
    ans := Int32.add !ans n
  done;
  !ans

let read_int32_le f =
  let s = read_string f 4 in
  let ans = ref Int32.zero in
  for i = 0 to 3 do
    let n = int_of_char s.[i] in
    let n = Int32.of_int n in
    let n = Int32.shift_left n (i * 8) in
    ans := Int32.add !ans n
  done;
  !ans

let bytes_of_int32 n =
  let s = Bytes.create 4 in
  let set i k =
    Bytes.set s i
      (char_of_int
         (Int32.to_int
            (Int32.logand (Int32.of_int 0xff) (Int32.shift_right n (8 * k)))))
  in
  set 0 3;
  set 1 2;
  set 2 1;
  set 3 0;
  Bytes.unsafe_to_string s

(** Writing. *)

let basic_header f ~chunk_type ~chunk_stream_id =
  assert (0 <= chunk_type && chunk_type < 4);
  assert (3 <= chunk_stream_id && chunk_stream_id <= 65599);
  if chunk_stream_id <= 63 then
    write_byte f ((chunk_type lsl 6) + chunk_stream_id)
  else if chunk_stream_id <= 319 then (
    write_byte f (chunk_type lsl 6);
    write_byte f (chunk_stream_id - 64) )
  else (
    write_byte f ((chunk_type lsl 6) + 0x111111);
    write_short f chunk_stream_id )

let int32_byte n k =
  Int32.to_int (Int32.logand (Int32.shift_right n (8 * k)) (Int32.of_int 0xff))

let chunk_header0 f ~chunk_stream_id ~timestamp ~message_length ~message_type_id
    ~message_stream_id =
  basic_header f ~chunk_type:0 ~chunk_stream_id;
  let extended = timestamp >= Int32.of_int 0xffffff in
  let timestamp' = if extended then 0xffffff else Int32.to_int timestamp in
  write_int24 f timestamp';
  write_int24 f message_length;
  write_byte f message_type_id;
  write_int32_le f message_stream_id;
  if extended then write_int32 f timestamp

let chunk_header1 f ~chunk_stream_id ~timestamp_delta ~message_length
    ~message_type_id =
  basic_header f ~chunk_type:1 ~chunk_stream_id;
  let extended = timestamp_delta >= Int32.of_int 0xffffff in
  let timestamp_delta' =
    if extended then 0xffffff else Int32.to_int timestamp_delta
  in
  write_int24 f timestamp_delta';
  write_int24 f message_length;
  write_byte f message_type_id;
  if extended then write_int32 f timestamp_delta

let chunk_header2 f ~chunk_stream_id ~timestamp_delta =
  basic_header f ~chunk_type:2 ~chunk_stream_id;
  let extended = timestamp_delta >= Int32.of_int 0xffffff in
  let timestamp_delta' =
    if extended then 0xffffff else Int32.to_int timestamp_delta
  in
  write_int24 f timestamp_delta';
  if extended then write_int32 f timestamp_delta

let chunk_header3 f ~chunk_stream_id =
  basic_header f ~chunk_type:3 ~chunk_stream_id

let control_message f message_stream_id payload =
  chunk_header0 f ~chunk_stream_id:0 ~timestamp:Int32.zero
    ~message_stream_id:(Int32.of_int 2) ~message_type_id:1
    ~message_length:(String.length payload)

let set_chunk_size f n =
  let s = Bytes.create 4 in
  Bytes.set s 0 (char_of_int ((n lsr 24) land 0xff));
  Bytes.set s 1 (char_of_int ((n lsr 16) land 0xff));
  Bytes.set s 2 (char_of_int ((n lsr 8) land 0xff));
  Bytes.set s 3 (char_of_int (n land 0xff));
  control_message f 1 (Bytes.unsafe_to_string s)

let abort_message f n = control_message f 2 (bytes_of_int32 n)
let acknowledgement f n = control_message f 3 (bytes_of_int32 n)
let window_acknowledgement_size f n = control_message f 5 (bytes_of_int32 n)

let set_peer_bandwidth f n t =
  let n = bytes_of_int32 n in
  let t = match t with `Hard -> 0 | `Soft -> 1 | `Dynamic -> 2 in
  let t = String.make 1 (char_of_int t) in
  control_message f 6 (n ^ t)

(* rtmp://a.rtmp.youtube.com/live2 *)
let test () =
  Random.self_init ();
  let server = "a.rtmp.youtube.com" in
  let addr = Unix.gethostbyname server in
  let s = Unix.socket addr.Unix.h_addrtype Unix.SOCK_STREAM 0 in
  Unix.connect s (Unix.ADDR_INET (addr.Unix.h_addr_list.(0), 1935));
  let time = Int32.zero in
  (* time origin *)
  (* C0 *)
  write_byte s 3;
  (* C1 *)
  write_int32 s time;
  write_int32 s Int32.zero;
  for i = 0 to 1527 do
    write_byte s (Random.int 256)
  done;
  (* S0 *)
  assert (read_byte s = 3);
  (* S1 *)
  assert (read_int32 s = time);
  let time2 = read_int32 s in
  let rand = read s 1528 in
  (* C2 *)
  write_int32 s time;
  write_int32 s time2;
  write s rand;
  (* S2 *)
  ignore (read s (4 + 4 + 1158))

let max_chunk_size = 128

type message = {
  message_chunk_stream_id : int;
  message_timestamp : Int32.t;
  message_type_id : int;
  message_stream_id : Int32.t;
  mutable message_data : string list;
  (* Data in chunks, to be read from right to left (i.e. rev before concatenating). *)
  mutable message_remaining : int; (* Bytes which remain to be fetched *)
}

(** Local server. *)
let () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 1935));
  Unix.listen s 5;
  while true do
    Printf.printf "Waiting for client\n%!";
    let s, caller = Unix.accept s in
    Printf.printf "Accepting connection!\n%!";
    (* C0 *)
    let c0 = read_byte s in
    assert (c0 = 3);
    Printf.printf "C0\n%!";
    (* S0 *)
    write_byte s 3;
    (* S1 *)
    let time2 = Int32.zero in
    let rand2 = Bytes.create 1528 in
    write_int32 s time2;
    write_int32 s Int32.zero;
    write s rand2;
    (* C1 *)
    let time = read_int32 s in
    let zero = read_int32 s in
    let rand = read s 1528 in
    (* assert (zero = Int32.zero); *)
    Printf.printf "C1\n%!";
    (* S2 *)
    write_int32 s time2;
    write_int32 s time;
    write s rand;
    (* C2 *)
    let time' = read_int32 s in
    if time' <> time then
      Printf.printf "C2 time: %ld instead of %ld\n%!" time' time;
    assert (read_int32 s = time2);
    assert (read s 1528 = rand2);
    Printf.printf "C2\n%!";
    let messages = ref [] in
    let add_message msg = messages := msg :: !messages in
    let find_message cid =
      List.find (fun msg -> msg.message_chunk_stream_id = cid) !messages
    in
    while true do
      Printf.printf "\n%!";
      (* Basic header *)
      let basic = read_byte s in
      let chunk_type = basic lsr 6 in
      Printf.printf "Chunk type: %d\n%!" chunk_type;
      let chunk_stream_id =
        let n = basic land 0x3f in
        if n = 0 then read_byte s + 64
        else if n = 0x3f then read_short s + 64
        else n
      in
      Printf.printf "Chunk stream id: %d\n%!" chunk_stream_id;
      (* Message header *)
      match chunk_type with
        | 0 ->
            let timestamp = read_int24 s in
            let message_length = read_int24 s in
            let message_type_id = read_byte s in
            let message_stream_id = read_int32_le s in
            let timestamp =
              if timestamp = 0xffffff then read_int32 s
              else Int32.of_int timestamp
            in
            Printf.printf "Timestamp: %d\n%!" (Int32.to_int timestamp);
            Printf.printf "Message length: %d\n%!" message_length;
            Printf.printf "Message type: %d (0x%x)\n%!" message_type_id
              message_type_id;
            Printf.printf "Message stream: %d\n%!"
              (Int32.to_int message_stream_id);
            let data_length = min max_chunk_size message_length in
            let data = read_string s data_length in
            let msg =
              {
                message_chunk_stream_id = chunk_stream_id;
                message_timestamp = timestamp;
                message_type_id;
                message_stream_id;
                message_data = [data];
                message_remaining = message_length - data_length;
              }
            in
            add_message msg
        | 3 ->
            let msg = find_message chunk_stream_id in
            let remaining = msg.message_remaining in
            let data_length = min max_chunk_size remaining in
            Printf.printf "read %d\n%!" data_length;
            let data = read_string s data_length in
            msg.message_data <- data :: msg.message_data;
            msg.message_remaining <- msg.message_remaining - data_length
        | _ -> assert false
    done;
    Printf.printf "Done with connection\n%!";
    exit 0
  done
