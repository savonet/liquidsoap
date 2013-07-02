(** Websockets. *)

(** A websocket frame. *)
type frame =
  {
    fin : bool;
    rsv1 : bool;
    rsv2 : bool;
    rsv3 : bool;
    opcode : int;
    data : string;
  }

(* Compute websocket anwser. *)
let wsa wsk =
  let wsa = wsk ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  Printf.printf "wsa: %s\n%!" wsa;
  let sha1 = Cryptokit.Hash.sha1 () in
  sha1#add_string wsa;
  let wsa = sha1#result in
  (* for i = 0 to String.length wsa - 1 do *)
    (* Printf.printf "%x" (int_of_char wsa.[i]) *)
  (* done; *)
  (* Printf.printf "\n%!"; *)
  (* TODO: the following returns a truncated answer *)
  (* let b64 = Cryptokit.Base64.encode_compact_pad () in *)
  (* b64#put_string wsa; *)
  (* let wsa = b64#get_string in *)
  let wsa = Utils.encode64 wsa in
  wsa

(** Handle an upgrade to websocket request. *)
let upgrade ~headers s =
  assert (List.assoc "Upgrade" headers = "websocket");
  let wsk = List.assoc "Sec-WebSocket-Key" headers in
  let wsa = wsa wsk in
  let ans = Printf.sprintf "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\n\r\n" wsa in
  Printf.printf "answer:\n%s\n%!" ans;
  assert (Unix.write s ans 0 (String.length ans) = String.length ans)

(** Read a websocket frame. *)
let read_frame s =
  let read_char () =
    let c = String.create 1 in
    let n = Unix.read s c 0 1 in
    assert (n = 1);
    c.[0]
  in
  let read_byte () =
    int_of_char (read_char ())
  in
  let read_short () =
    let c1 = read_byte () in
    let c2 = read_byte () in
    c1 lsl 8 + c2
  in
  let read_long () =
    let c1 = read_byte () in
    let c2 = read_byte () in
    let c3 = read_byte () in
    let c4 = read_byte () in
    c1 lsl 24 + c2 lsl 16 + c3 lsl 8 + c4
  in
  let c = read_byte () in
  let fin = c land 0b10000000 <> 0 in
  let rsv1 = c land 0b1000000 <> 0 in
  let rsv2 = c land 0b100000 <> 0 in
  let rsv3 = c land 0b10000 <> 0 in
  let opcode = c land 0b1111 in
  (* Printf.printf "fin: %B\n%!" fin; *)
  (* Printf.printf "rsv: %B %B %B\n%!" rsv1 rsv2 rsv3; *)
  (* Printf.printf "op: 0x%x\n%!" opcode; *)
  let c = read_byte () in
  let mask = c land 0b10000000 <> 0 in
  let length = c land 0b1111111 in
  (* Printf.printf "mask: %B\n%!" mask; *)
  (* TODO: is this right? *)
  let length =
    if length = 126 then
      read_short ()
    else if length = 127 then
      read_long ()
    else
      length
  in
  (* Printf.printf "length: %d\n%!" length; *)
  let masking_key =
    if mask then
      let key = String.create 4 in
      for i = 0 to 3 do
        key.[i] <- read_char ()
      done;
      key
    else
      ""
  in
  let unmask key s =
    if key <> "" then
      for i = 0 to String.length s - 1 do
        let c = int_of_char s.[i] in
        let k = int_of_char key.[i mod 4] in
        let c = c lxor k in
        let c = char_of_int c in
        s.[i] <- c
      done
  in
  (* Printf.printf "masking key: %s\n%!" masking_key; *)
  let data = String.create length in
  let n = Unix.read s data 0 length in
  assert (n = length);
  unmask masking_key data;
  (* Printf.printf "data: %s\n%!" data; *)
  { fin; rsv1; rsv2; rsv3; opcode; data }
