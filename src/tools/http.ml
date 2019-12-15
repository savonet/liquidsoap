module type Transport_t = sig
  type connection

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  val default_port : int
  val connect : ?bind_address:string -> string -> int -> connection
  val wait_for : ?log:(string -> unit) -> event -> float -> unit
  val write : connection -> Bytes.t -> int -> int -> int
  val read : connection -> Bytes.t -> int -> int -> int
  val disconnect : connection -> unit
end

module Unix_transport : Transport_t with type connection = Unix.file_descr =
struct
  type connection = Unix.file_descr

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  exception Socket

  let connect ?bind_address host port =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    begin
      match bind_address with
      | None -> ()
      | Some s ->
          let bind_addr_inet = (Unix.gethostbyname s).Unix.h_addr_list.(0) in
          (* Seems like you need to bind on port 0 *)
          let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
          Unix.bind socket bind_addr
    end;
    try
      Unix.connect socket
        (Unix.ADDR_INET ((Unix.gethostbyname host).Unix.h_addr_list.(0), port));
      socket
    with _ ->
      Unix.close socket;
      raise Socket

  let default_port = 80
  let wait_for = Tutils.wait_for
  let write = Unix.write
  let read = Unix.read
  let disconnect socket = try Unix.close socket with _ -> ()
end

module type Http_t = sig
  (** Error handling *)
  type error = Socket | Response | UrlDecoding

  exception Error of error

  val string_of_error : error -> string

  type connection

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  type uri = { host : string; port : int option; path : string }

  val default_port : int
  val user_agent : string
  val url_decode : ?plus:bool -> string -> string
  val url_encode : ?plus:bool -> string -> string
  val parse_url : string -> uri
  val is_url : string -> bool
  val dirname : string -> string
  val args_split : string -> (string, string) Hashtbl.t
  val connect : ?bind_address:string -> string -> int -> connection
  val disconnect : connection -> unit
  val read : connection -> Bytes.t -> int -> int -> int
  val write : connection -> Bytes.t -> int -> int -> int
  val wait_for : ?log:(string -> unit) -> event -> float -> unit

  type status = string * int * string
  type headers = (string * string) list

  val read_crlf :
    ?log:(string -> unit) ->
    ?max:int ->
    ?count:int ->
    timeout:float ->
    connection ->
    string

  val read_chunked : timeout:float -> connection -> string * int

  val request :
    ?log:(string -> unit) ->
    timeout:float ->
    connection ->
    string ->
    (string * int * string) * (string * string) list

  val get :
    ?headers:(string * string) list ->
    ?log:(string -> unit) ->
    timeout:float ->
    connection ->
    uri ->
    (string * int * string) * (string * string) list

  val post :
    ?headers:(string * string) list ->
    ?log:(string -> unit) ->
    timeout:float ->
    string ->
    connection ->
    uri ->
    (string * int * string) * (string * string) list

  val put :
    ?headers:(string * string) list ->
    ?log:(string -> unit) ->
    timeout:float ->
    string ->
    connection ->
    uri ->
    (string * int * string) * (string * string) list

  val head :
    ?headers:(string * string) list ->
    ?log:(string -> unit) ->
    timeout:float ->
    connection ->
    uri ->
    (string * int * string) * (string * string) list

  val delete :
    ?headers:(string * string) list ->
    ?log:(string -> unit) ->
    timeout:float ->
    connection ->
    uri ->
    (string * int * string) * (string * string) list

  val read_with_timeout :
    ?log:(string -> unit) -> timeout:float -> connection -> int option -> string

  type request = Get | Post of string | Put of string | Head | Delete

  val full_request :
    ?headers:(string * string) list ->
    ?log:(string -> unit) ->
    timeout:float ->
    uri:uri ->
    request:request ->
    unit ->
    (string * int * string) * (string * string) list * string
end

module Make (Transport : Transport_t) = struct
  (* Some structured exceptions *)

  include Extralib

  type error = Socket | Response | UrlDecoding

  exception Error of error

  let string_of_error e =
    match e with
      | Socket -> "Http: error while communicating to socket"
      | Response -> "Http: invalid answer to request"
      | UrlDecoding -> "Http: URL decoding failed"

  (** Error translator *)
  let error_translator (e : exn) =
    match e with Error e -> Some (string_of_error e) | _ -> None

  type connection = Transport.connection

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  type uri = { host : string; port : int option; path : string }

  let () = Printexc.register_printer error_translator
  let raise e = raise (Error e)
  let default_port = Transport.default_port
  let user_agent = Configure.vendor

  (* URL encoding/decoding according to RFC 1738, RFC 1630.
   * Borrowed from ocamlnet. *)

  (** Converts k to a 2-digit hexadecimal string. *)
  let to_hex2 =
    let hex_digits =
      [|
        '0';
        '1';
        '2';
        '3';
        '4';
        '5';
        '6';
        '7';
        '8';
        '9';
        'A';
        'B';
        'C';
        'D';
        'E';
        'F';
      |]
    in
    fun k ->
      let s = Bytes.create 2 in
      Bytes.set s 0 hex_digits.((k lsr 4) land 15);
      Bytes.set s 1 hex_digits.(k land 15);
      Bytes.unsafe_to_string s

  let url_encode ?(plus = true) s =
    Pcre.substitute ~pat:"[^A-Za-z0-9_.!*-]"
      ~subst:(fun x ->
        if plus && x = " " then "+"
        else (
          let k = Char.code x.[0] in
          "%" ^ to_hex2 k ))
      s

  let of_hex1 c =
    match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | _ -> raise UrlDecoding

  let url_decode ?(plus = true) s =
    Pcre.substitute
      ~pat:
        "\\+|%..|%.|%"
        (* TODO why do we match %. and % and seem to exclude them below ? *)
      ~subst:(fun s ->
        if s = "+" then if plus then " " else "+"
        else (
          (* Assertion: s.[0] = '%' *)
          if String.length s < 3 then raise UrlDecoding;
          let k1 = of_hex1 s.[1] in
          let k2 = of_hex1 s.[2] in
          String.make 1 (Char.chr ((k1 lsl 4) lor k2)) ))
      s

  let args_split s =
    let args = Hashtbl.create 2 in
    let fill_arg arg =
      let arg = url_decode arg in
      match Pcre.split ~pat:"=" arg with
        | e :: l ->
            (* There should be only arg=value *)
            List.iter (Hashtbl.replace args e) l
        | [] -> ()
    in
    List.iter fill_arg (Pcre.split ~pat:"&" s);
    args

  let connect = Transport.connect
  let disconnect = Transport.disconnect
  let read = Transport.read
  let write = Transport.write
  let wait_for = Transport.wait_for

  (* exception Invalid_url *)

  let parse_url url =
    let basic_rex =
      Pcre.regexp "^[Hh][Tt][Tt][Pp][sS]?://([^/:]+)(:[0-9]+)?(/.*)?$"
    in
    let sub =
      try Pcre.exec ~rex:basic_rex url
      with Not_found -> (* raise Invalid_url *)
                        failwith "Invalid URL."
    in
    let host = Pcre.get_substring sub 1 in
    let port =
      try
        let port = Pcre.get_substring sub 2 in
        let port = String.sub port 1 (String.length port - 1) in
        let port = int_of_string port in
        Some port
      with Not_found -> None
    in
    let path = try Pcre.get_substring sub 3 with Not_found -> "/" in
    { host; port; path }

  let is_url path = Pcre.pmatch ~pat:"^[Hh][Tt][Tt][Pp][sS]?://.+" path

  let dirname url =
    let rex = Pcre.regexp "^([Hh][Tt][Tt][Pp][sS]?://.+/)[^/]*$" in
    let s = Pcre.exec ~rex url in
    Pcre.get_substring s 1

  let read_with_timeout ?(log = fun _ -> ()) ~timeout socket buflen =
    Transport.wait_for ~log (`Read socket) timeout;
    match buflen with
      | Some buflen ->
          let buf = Bytes.create buflen in
          let n = Transport.read socket buf 0 buflen in
          Bytes.sub_string buf 0 n
      | None ->
          let buflen = Utils.pagesize in
          let buf = Bytes.create buflen in
          let ans = ref "" in
          let n = ref buflen in
          while !n <> 0 do
            n := Transport.read socket buf 0 buflen;
            ans := !ans ^ Bytes.sub_string buf 0 !n
          done;
          !ans

  type status = string * int * string
  type headers = (string * string) list

  (* An ugly code to read until we see [\r]?\n n times. *)
  let read_crlf ?(log = fun _ -> ()) ?(max = 4096) ?(count = 2) ~timeout socket
      =
    (* We read until we see [\r]?\n n times *)
    let ans = Buffer.create 10 in
    let n = ref 0 in
    let count_n = ref 0 in
    let stop = ref false in
    let c = Bytes.create 1 in
    (* We need to parse char by char because
     * we want to make sure we stop at the exact
     * end of [\r]?\n in order to pass a socket
     * which is placed at the exact char after it.
     * The maximal length is a security but it may
     * be lifted.. *)
    while !count_n < count && !n < max && not !stop do
      (* This is quite ridiculous but we have 
       * no way to know how much data is available
       * in the socket.. *)
      Transport.wait_for ~log (`Read socket) timeout;
      let h = Transport.read socket c 0 1 in
      if h < 1 then stop := true
      else (
        let c = Bytes.get c 0 in
        Buffer.add_char ans c;
        if c = '\n' then incr count_n else if c <> '\r' then count_n := 0 );
      incr n
    done;
    Buffer.contents ans

  (* Read chunked transfer. *)
  let read_chunked ~timeout socket =
    let read = read_crlf ~count:1 ~timeout socket in
    let len = List.hd (Pcre.split ~pat:"[\r]?\n" read) in
    let len = List.hd (Pcre.split ~pat:";" len) in
    let len = int_of_string ("0x" ^ len) in
    let buf = Buffer.create len in
    let rec f () =
      let rem = len - Buffer.length buf in
      assert (0 < rem);
      let s = Bytes.create rem in
      let n = Transport.read socket s 0 rem in
      Buffer.add_subbytes buf s 0 n;
      if Buffer.length buf = len then Buffer.contents buf else f ()
    in
    let s = f () in
    ignore (read_crlf ~count:1 ~timeout socket);
    (s, len)

  let request ?(log = fun _ -> ()) ~timeout socket request =
    if
      let len = String.length request in
      Transport.wait_for ~log (`Write socket) timeout;
      Transport.write socket (Bytes.of_string request) 0 len < len
    then raise Socket;
    let header = read_crlf ~log ~timeout socket in
    let header = Pcre.split ~pat:"[\r]?\n" header in
    let response, header =
      match header with e :: tl -> (e, tl) | [] -> raise Response
    in
    let response_http_version, response_status, response_msg =
      let pat = "^((?:HTTP/[0-9.]+)|ICY) ([0-9]+) (.*)$" in
      try
        let ( !! ) = Pcre.get_substring (Pcre.exec ~pat response) in
        (!!1, int_of_string !!2, !!3)
      with Not_found -> raise Response
    in
    let fields =
      let pat = "([^:]*):\\s*(.*)" in
      List.fold_left
        (fun fields line ->
          try
            let ( !! ) = Pcre.get_substring (Pcre.exec ~pat line) in
            (String.lowercase_ascii !!1, !!2) :: fields
          with Not_found -> fields)
        [] header
    in
    ((response_http_version, response_status, response_msg), List.rev fields)

  type request = Get | Post of string | Put of string | Head | Delete

  let method_of_request = function
    | Get -> "GET"
    | Post _ -> "POST"
    | Put _ -> "PUT"
    | Head -> "HEAD"
    | Delete -> "DELETE"

  let data_of_request = function Post d | Put d -> d | _ -> assert false

  let http_req ?(headers = []) request uri =
    let req =
      Printf.sprintf "%s %s HTTP/1.0\r\n" (method_of_request request) uri.path
    in
    let req =
      match uri.port with
        | None -> Printf.sprintf "%sHost: %s\r\n" req uri.host
        | Some port -> Printf.sprintf "%sHost: %s:%d\r\n" req uri.host port
    in
    let req =
      if not (List.mem_assoc "User-Agent" headers) then
        Printf.sprintf "%sUser-Agent: %s\r\n" req user_agent
      else req
    in
    let req =
      List.fold_left
        (fun s (t, v) -> Printf.sprintf "%s%s: %s\r\n" s t v)
        req headers
    in
    if not (List.mem request [Get; Head; Delete]) then (
      let data = data_of_request request in
      Printf.sprintf "%sContent-Length: %d\r\n\r\n%s\r\n" req
        (String.length data) data )
    else Printf.sprintf "%s\r\n" req

  let execute ?(headers = []) ?log ~timeout socket action uri =
    let req = http_req ~headers action uri in
    request ?log ~timeout socket req

  let get ?headers ?log ~timeout socket uri =
    execute ?headers ?log ~timeout socket Get uri

  let head ?headers ?log ~timeout socket uri =
    execute ?headers ?log ~timeout socket Head uri

  let delete ?headers ?log ~timeout socket uri =
    execute ?headers ?log ~timeout socket Delete uri

  let post ?headers ?log ~timeout data socket uri =
    execute ?headers ?log ~timeout socket (Post data) uri

  let put ?headers ?log ~timeout data socket uri =
    execute ?headers ?log ~timeout socket (Put data) uri

  let full_request ?headers ?(log = fun _ -> ()) ~timeout ~uri ~request () =
    let port = match uri.port with Some port -> port | None -> default_port in
    let connection = Transport.connect uri.host port in
    Tutils.finalize
      ~k:(fun () -> Transport.disconnect connection)
      (fun () ->
        let execute request =
          execute ?headers ~log ~timeout connection request uri
        in
        (* We raise an error if the statuses are not correct. *)
        let status, headers = execute request in
        let max =
          try
            let _, len =
              List.find
                (fun (l, _) -> String.lowercase_ascii l = "content-length")
                headers
            in
            int_of_string len
          with _ -> max_int
        in
        let ret = read_crlf ~log ~timeout ~max connection in
        ( status,
          headers,
          Pcre.substitute ~pat:"[\r]?\n$" ~subst:(fun _ -> "") ret ))
end

module Http_request = Make (Unix_transport)
include Http_request
