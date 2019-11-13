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

  val read : socket -> msg

  val write : socket -> msg -> unit

  val upgrade : (string * string) list -> string
end

module Make (T : Transport_t) : Websocket_t with type socket = T.socket =
struct
  type socket = T.socket

  type msg =
    [ `Binary of string
    | `Close of (int * string) option
    | `Ping of string
    | `Pong of string
    | `Text of string ]

  (* Compute websocket anwser. *)
  let wsa wsk =
    let wsa = wsk ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
    let wsa = Sha1.digest wsa in
    let wsa = Utils.encode64 wsa in
    wsa

  (** Handle an upgrade to websocket request. *)
  let upgrade headers =
    assert (List.assoc "Upgrade" headers = "websocket") ;
    let origin =
      try Printf.sprintf "Origin: %s\r\n" (List.assoc "Origin" headers)
      with _ -> ""
    in
    let version =
      try
        let value = List.assoc "Sec-WebSocket-Version" headers in
        Printf.sprintf "Sec-WebSocket-Version: %s\r\n" value
      with Not_found -> ""
    in
    let wsk = List.assoc "Sec-WebSocket-Key" headers in
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
      fin: bool;
      rsv1: bool;
      rsv2: bool;
      rsv3: bool;
      opcode: int;
      data: string;
    }

    (** Forge a frame. *)
    let to_string f =
      let bit b = if b then 1 else 0 in
      let len = String.length f.data in
      let b0 =
        (bit f.fin lsl 7)
        lor (bit f.rsv1 lsl 6)
        lor (bit f.rsv2 lsl 5)
        lor f.opcode
      in
      let b0 = char_of_int b0 in
      let b0 = String.make 1 b0 in
      let blen =
        if len <= 125 then String.make 1 (char_of_int len)
        else if len <= 0xffff then (
          let ans = Bytes.create 3 in
          Bytes.set ans 0 '\126' ;
          Bytes.set ans 1 (char_of_int (len lsr 8)) ;
          Bytes.set ans 2 (char_of_int (len land 0xff)) ;
          Bytes.unsafe_to_string ans )
        else (
          let ans = Bytes.create 5 in
          Bytes.set ans 0 '\127' ;
          Bytes.set ans 1 (char_of_int ((len lsr 24) land 0xff)) ;
          Bytes.set ans 2 (char_of_int ((len lsr 16) land 0xff)) ;
          Bytes.set ans 3 (char_of_int ((len lsr 8) land 0xff)) ;
          Bytes.set ans 4 (char_of_int (len land 0xff)) ;
          Bytes.unsafe_to_string ans )
      in
      b0 ^ blen ^ f.data

    (** Read a websocket frame. *)
    let read s =
      let read_char () =
        let c = Bytes.create 1 in
        let n = T.read s c 0 1 in
        assert (n = 1) ;
        Bytes.get c 0
      in
      let read_byte () = int_of_char (read_char ()) in
      let read_short () =
        let c1 = read_byte () in
        let c2 = read_byte () in
        (c1 lsl 8) + c2
      in
      let read_long () =
        let c1 = read_byte () in
        let c2 = read_byte () in
        let c3 = read_byte () in
        let c4 = read_byte () in
        (c1 lsl 24) + (c2 lsl 16) + (c3 lsl 8) + c4
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
        else length
      in
      let masking_key =
        if mask then (
          let key = Bytes.create 4 in
          for i = 0 to 3 do
            Bytes.set key i (read_char ())
          done ;
          Bytes.unsafe_to_string key )
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
      let data = Bytes.create length in
      let n = T.read_retry s data 0 length in
      assert (n = length) ;
      unmask masking_key data ;
      let data = Bytes.unsafe_to_string data in
      {fin; rsv1; rsv2; rsv3; opcode; data}
  end

  let rec read s =
    let frame = Frame.read s in
    let data = frame.Frame.data in
    (* TODO: handle continuation frames. There is a problem with our model though:
       control frames can be inserted in the middle of a fragmented packet. *)
    assert (frame.Frame.fin = true) ;
    match frame.Frame.opcode with
      | 0x1 ->
          `Text data
      | 0x2 ->
          `Binary data
      | 0x8 ->
          let reason =
            if data = "" then None
            else (
              assert (String.length data >= 2) ;
              let code = (int_of_char data.[0] lsl 8) + int_of_char data.[1] in
              Some (code, String.sub data 2 (String.length data - 2)) )
          in
          `Close reason
      | 0x9 ->
          `Ping data
      | 0xa ->
          `Pong data
      | _ ->
          read s

  let to_string data =
    let frame =
      {
        Frame.fin= true;
        rsv1= false;
        rsv2= false;
        rsv3= false;
        opcode= 0;
        data= "";
      }
    in
    let frame =
      match data with
        | `Text s ->
            {frame with Frame.opcode= 0x1; data= s}
        | `Binary data ->
            {frame with Frame.opcode= 0x2; data}
        | `Close r ->
            let data =
              match r with
                | None ->
                    ""
                | Some (n, msg) ->
                    let nn = Bytes.create 2 in
                    Bytes.set nn 0 (char_of_int (n lsr 8)) ;
                    Bytes.set nn 1 (char_of_int (n land 0xff)) ;
                    Bytes.unsafe_to_string nn ^ msg
            in
            {frame with Frame.opcode= 0x8; data}
        | `Ping data ->
            {frame with Frame.opcode= 0x9; data}
        | `Pong data ->
            {frame with Frame.opcode= 0xa; data}
        | _ ->
            assert false
    in
    Frame.to_string frame

  let write s data =
    let frame = to_string data in
    let n = T.write s (Bytes.of_string frame) 0 (String.length frame) in
    assert (n = String.length frame)
end

module Websocket = Make (Unix_transport)
include Websocket
