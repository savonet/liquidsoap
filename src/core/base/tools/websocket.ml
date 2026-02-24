(** Websockets. *)

type 'a unix_opt = 'a -> Bytes.t -> int -> int -> int

module type Transport_t = sig
  type socket

  val read : socket unix_opt
  val read_retry : socket unix_opt
  val write : socket unix_opt
end

module Unix_transport = struct
  type socket = Unix.file_descr

  let read = Unix.read
  let read_retry = Extralib.Unix.read_retry
  let write = Unix.write
end

module type Websocket_t = sig
  type socket

  type msg =
    [ `Binary of string
    | `Close of (int * string) option
    | `Ping of string
    | `Pong of string
    | `Text of string ]

  val to_string : msg -> string
  val read : unit -> socket -> msg
  val write : socket -> msg -> unit
  val upgrade : (string * string) list -> string
end

module Make (T : Transport_t) : Websocket_t with type socket = T.socket = struct
  type socket = T.socket

  type msg =
    [ `Binary of string
    | `Close of (int * string) option
    | `Ping of string
    | `Pong of string
    | `Text of string ]

  (* Compute websocket answer. *)
  let wsa wsk =
    let wsa = wsk ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
    let wsa = Sha1.digest wsa in
    let wsa = Lang_string.encode64 wsa in
    wsa

  (** Handle an upgrade to websocket request. *)
  let upgrade headers =
    let headers =
      List.map (fun (lbl, value) -> (String.uppercase_ascii lbl, value)) headers
    in
    assert (List.assoc "UPGRADE" headers = "websocket");
    let origin =
      try Printf.sprintf "Origin: %s\r\n" (List.assoc "ORIGIN" headers)
      with _ -> ""
    in
    let version =
      try
        let value = List.assoc "SEC-WEBSOCKET-VERSION" headers in
        Printf.sprintf "Sec-WebSocket-Version: %s\r\n" value
      with Not_found -> ""
    in
    let wsk = List.assoc "SEC-WEBSOCKET-KEY" headers in
    let wsa = wsa wsk in
    Printf.sprintf
      "HTTP/1.1 101 Switching Protocols\r\n\
       Connection: Upgrade\r\n\
       %sSec-WebSocket-Accept: %s\r\n\
       Sec-WebSocket-Protocol: webcast\r\n\
       %sUpgrade: websocket\r\n\
       \r\n"
      origin wsa version

  (** Websocket frames. *)
  module Frame = struct
    (** A websocket frame. *)
    type t = {
      fin : bool;
      rsv1 : bool;
      rsv2 : bool;
      rsv3 : bool;
      opcode : int;
      data : string;
    }

    (** Forge a frame. *)
    let to_string f =
      let bit b = if b then 1 else 0 in
      let len = String.length f.data in
      let b0 =
        (bit f.fin lsl 7)
        lor (bit f.rsv1 lsl 6)
        lor (bit f.rsv2 lsl 5)
        lor (bit f.rsv3 lsl 4)
        lor f.opcode
      in
      let b0 = char_of_int b0 in
      let b0 = String.make 1 b0 in
      let blen =
        if len <= 125 then String.make 1 (char_of_int len)
        else if len <= 0xffff then (
          let ans = Bytes.create 3 in
          Bytes.set ans 0 '\126';
          Bytes.set ans 1 (char_of_int (len lsr 8));
          Bytes.set ans 2 (char_of_int (len land 0xff));
          Bytes.unsafe_to_string ans)
        else (
          let ans = Bytes.create 5 in
          Bytes.set ans 0 '\127';
          Bytes.set ans 1 (char_of_int ((len lsr 24) land 0xff));
          Bytes.set ans 2 (char_of_int ((len lsr 16) land 0xff));
          Bytes.set ans 3 (char_of_int ((len lsr 8) land 0xff));
          Bytes.set ans 4 (char_of_int (len land 0xff));
          Bytes.unsafe_to_string ans)
      in
      b0 ^ blen ^ f.data

    (** Read a websocket frame. *)
    let read s =
      let read_char () =
        let c = Bytes.create 1 in
        let n = T.read s c 0 1 in
        assert (n = 1);
        Bytes.get c 0
      in
      let read_byte () = int_of_char (read_char ()) in
      let read_short () =
        let c1 = read_byte () in
        let c2 = read_byte () in
        Int64.of_string (Printf.sprintf "0x%02x%02x" c1 c2)
      in
      let read_long () =
        let c1 = read_byte () in
        let c2 = read_byte () in
        let c3 = read_byte () in
        let c4 = read_byte () in
        let c5 = read_byte () in
        let c6 = read_byte () in
        let c7 = read_byte () in
        let c8 = read_byte () in
        Int64.of_string
          (Printf.sprintf "0x%02x%02x%02x%02x%02x%02x%02x%02x" c1 c2 c3 c4 c5 c6
             c7 c8)
      in
      let c = read_byte () in
      let fin = c land 0b10000000 <> 0 in
      let rsv1 = c land 0b1000000 <> 0 in
      let rsv2 = c land 0b100000 <> 0 in
      let rsv3 = c land 0b10000 <> 0 in
      let opcode = c land 0b1111 in
      let c = read_byte () in
      let mask = c land 0b10000000 <> 0 in
      let length = c land 0b1111111 in
      let length =
        if length = 126 then read_short ()
        else if length = 127 then read_long ()
        else Int64.of_int length
      in
      let masking_key =
        if mask then (
          let key = Bytes.create 4 in
          for i = 0 to 3 do
            Bytes.set key i (read_char ())
          done;
          Bytes.unsafe_to_string key)
        else ""
      in
      let unmask key s =
        if String.length key > 0 then
          for i = 0 to Bytes.length s - 1 do
            let c = int_of_char (Bytes.get s i) in
            let k = int_of_char key.[i mod 4] in
            let c = c lxor k in
            let c = char_of_int c in
            Bytes.set s i c
          done
      in
      let buflen = Utils.buflen in
      let buf = Buffer.create buflen in
      let rec f pos =
        if pos < length then (
          let len = min buflen (Int64.to_int (Int64.sub length pos)) in
          let data = Bytes.create len in
          let n = T.read s data 0 len in
          Buffer.add_subbytes buf data 0 n;
          if n = 0 then failwith "end of stream reached prematurely!";
          f (Int64.add pos (Int64.of_int n)))
        else Buffer.to_bytes buf
      in
      let data = f 0L in
      unmask masking_key data;
      let data = Bytes.unsafe_to_string data in
      { fin; rsv1; rsv2; rsv3; opcode; data }
  end

  let read () =
    (* This queue stores control frames which are in the middle of a fragmented
       packet so that they can be handled later on. This is why the function
       takes unit as argument in order to create the closure. *)
    let frames = Queue.create () in
    let rec read s =
      let frame =
        if Queue.is_empty frames then Frame.read s else Queue.pop frames
      in
      let data =
        match frame.Frame.fin with
          | true -> frame.Frame.data
          | false ->
              let buf = Buffer.create Utils.buflen in
              Buffer.add_string buf frame.Frame.data;
              let rec continuation () =
                let frame = Frame.read s in
                match frame.Frame.opcode with
                  | 0 ->
                      Buffer.add_string buf frame.Frame.data;
                      if frame.Frame.fin then Buffer.contents buf
                      else continuation ()
                  | _ ->
                      Queue.push frame frames;
                      continuation ()
              in
              continuation ()
      in
      match frame.Frame.opcode with
        | 0x1 -> `Text data
        | 0x2 -> `Binary data
        | 0x8 ->
            let reason =
              if data = "" then None
              else (
                assert (String.length data >= 2);
                let code =
                  (int_of_char data.[0] lsl 8) + int_of_char data.[1]
                in
                Some (code, String.sub data 2 (String.length data - 2)))
            in
            `Close reason
        | 0x9 -> `Ping data
        | 0xa -> `Pong data
        | _ -> read s
    in
    read

  let to_string data =
    let frame =
      {
        Frame.fin = true;
        rsv1 = false;
        rsv2 = false;
        rsv3 = false;
        opcode = 0;
        data = "";
      }
    in
    let frame =
      match data with
        | `Text s -> { frame with Frame.opcode = 0x1; data = s }
        | `Binary data -> { frame with Frame.opcode = 0x2; data }
        | `Close r ->
            let data =
              match r with
                | None -> ""
                | Some (n, msg) ->
                    let nn = Bytes.create 2 in
                    Bytes.set nn 0 (char_of_int (n lsr 8));
                    Bytes.set nn 1 (char_of_int (n land 0xff));
                    Bytes.unsafe_to_string nn ^ msg
            in
            { frame with Frame.opcode = 0x8; data }
        | `Ping data -> { frame with Frame.opcode = 0x9; data }
        | `Pong data -> { frame with Frame.opcode = 0xa; data }
        | _ -> assert false
    in
    Frame.to_string frame

  let write s data =
    let frame = to_string data in
    let n = T.write s (Bytes.of_string frame) 0 (String.length frame) in
    assert (n = String.length frame)
end

module Websocket = Make (Unix_transport)
include Websocket
