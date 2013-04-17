(* See http://tools.ietf.org/html/rfc6455 *)

let port = 1234

module List = struct
  include List

  let may_map f l =
    let rec aux = function
      | x::t ->
        (
          match f x with
          | Some x -> x::(aux t)
          | None -> aux t
        )
      | [] -> []
    in
    aux l
end

(** Base 64 encoding. *)
let encode64 s =
  let digit =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let extra = String.length s mod 3 in
  let s = match extra with 1 -> s ^ "\000\000" | 2 -> s ^ "\000" | _ -> s in
  let n = String.length s in
  let dst = String.create (4 * (n/3)) in
  for i = 0 to n/3 - 1 do
    let (:=) j v = dst.[i*4+j] <- digit.[v] in
    let c j = int_of_char s.[i*3+j] in
    let c0 = c 0 and c1 = c 1 and c2 = c 2 in
    0 := c0 lsr 2 ;
    1 := ((c0 lsl 4) land 63) lor (c1 lsr 4) ;
    2 := ((c1 lsl 2) land 63) lor (c2 lsr 6) ;
    3 := c2 land 63
  done ;
  if extra = 1 then begin
    dst.[4*(n/3)-2] <- '=' ;
    dst.[4*(n/3)-1] <- '='
  end else if extra = 2 then
      dst.[4*(n/3)-1] <- '=' ;
  dst

let wsa wsk =
  let wsa = wsk ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  Printf.printf "wsa: %s\n%!" wsa;
  let sha1 = Cryptokit.Hash.sha1 () in
  sha1#add_string wsa;
  let wsa = sha1#result in
  for i = 0 to String.length wsa - 1 do
    Printf.printf "%x" (int_of_char wsa.[i])
  done;
  Printf.printf "\n%!";
  (* TODO: the following returns a truncated answer *)
  (* let b64 = Cryptokit.Base64.encode_compact_pad () in *)
  (* b64#put_string wsa; *)
  (* let wsa = b64#get_string in *)
  let wsa = encode64 wsa in
  wsa

let () =
  Printf.printf "test WSA: %s\n%!" (wsa "x3JJHMbDL1EzLkh9GBhXDw==")

let frame s =
  let read_char () =
    let c = String.create 1 in
    assert (Unix.read s c 0 1 = 1);
    c.[0]
  in
  let read_byte () =
    let c = int_of_char (read_char ()) in
    Printf.printf "byte: %d ('%c')\n%!" c (char_of_int c);
    c
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
  let msb c = c land 128 <> 0 in
  let read_len () =
    let len = ref 0 in
    let loop = ref true in
    while !loop do
      let c = read_byte () in
      len := !len * 128 + c land 127;
      loop := msb c
    done;
    !len
  in
  (*
    let t = read_byte () in
    Printf.printf "type: %d\n%!" t;
    Printf.printf "msb: %B\n%!" (msb t);
  (* TODO: raw data otherwise *)
    assert (msb t);
    let len = read_len () in
    Printf.printf "length: %d\n%!" len;
    let buf = String.create len in
    assert (Unix.read s buf 0 len = len);
    Printf.printf "read: %s\n%!" buf
  *)
  let c = read_byte () in
  let fin = c land 0b10000000 <> 0 in
  let rsv1 = c land 0b1000000 <> 0 in
  let rsv2 = c land 0b100000 <> 0 in
  let rsv3 = c land 0b10000 <> 0 in
  let opcode = c land 0b1111 in
  Printf.printf "fin: %B\n%!" fin;
  Printf.printf "rsv: %B %B %B\n%!" rsv1 rsv2 rsv3;
  Printf.printf "op: 0x%x\n%!" opcode;
  let c = read_byte () in
  let mask = c land 0b10000000 <> 0 in
  let length = c land 0b1111111 in
  Printf.printf "mask: %B\n%!" mask;
  let length =
    if length = 126 then
      read_short ()
    else if length = 127 then
      read_long()
    else
      length
  in
  Printf.printf "length: %d\n%!" length;
  let masking_key =
    if mask then
      let key = String.create 4 in
      for i = 0 to 3 do
        key.[i] <- read_char()
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
  assert (Unix.read s data 0 length = length);
  unmask masking_key data;
  Printf.printf "data: %s\n%!" data;
  ()

let handle s =
  let buflen = 1024 in
  let buf = String.create buflen in
  let headers =
    let n = Unix.read s buf 0 buflen in
    String.sub buf 0 n
  in
  Printf.printf "*** Headers:\n%s\n%!" headers;
  let headers = Str.split (Str.regexp "\r\n") headers in
  List.iter (fun h -> Printf.printf "header: '%s'\n%!" h) headers;
  let headers =
    let re = Str.regexp "^\\(.*\\): \\(.*\\)$" in
    List.may_map (fun h ->
      if Str.string_match re h 0 then
        let l = Str.matched_group 1 h in
        let v = Str.matched_group 2 h in
        Some (l,v)
      else
        None) headers
  in
  List.iter (fun (l,v) -> Printf.printf "header: %s / %s\n%!" l v) headers;
  let wsk = List.assoc "Sec-WebSocket-Key" headers in
  let wsa = wsa wsk in
  let ans = Printf.sprintf "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\n\r\n" wsa in
  Printf.printf "answer:\n%s\n%!" ans;
  assert (Unix.write s ans 0 (String.length ans) = String.length ans);
  while true do
    (* let n = Unix.read s buf 0 buflen in *)
    (* Printf.printf "Received:\n%s\n%!" (String.sub buf 0 buflen) *)
    frame s
  done

let () =
  Printf.printf "Creating server\n%!";
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, port) in
  let domain =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
    | Unix.ADDR_INET (_, _) -> Unix.PF_INET
  in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let handle_connexion (s,caller) =
    Printf.printf "Handle connection.\n%!";
    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) -> n
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_any
    in
    let inet_addr = inet_addr_of_sockaddr caller in
    Printf.printf "Openning connection for %s.\n%!" (Unix.string_of_inet_addr inet_addr);
    handle s
  in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 100;
  Printf.printf "Listening for connections.\n%!";
  while true do
    let (s, caller) = Unix.accept sock in
    ignore (Thread.create handle_connexion (s, caller));
  done
