type uri = {
  protocol : string;
  host : string;
  port : int option;
  path : string;
}

type event = [ `Write | `Read | `Both ]

type socket =
  < typ : string
  ; transport : transport
  ; file_descr : Unix.file_descr
  ; wait_for : ?log:(string -> unit) -> event -> float -> unit
  ; write : Bytes.t -> int -> int -> int
  ; read : Bytes.t -> int -> int -> int
  ; close : unit >

and transport =
  < name : string
  ; protocol : string
  ; default_port : int
  ; connect : ?bind_address:string -> ?timeout:float -> string -> int -> socket
  ; accept : Unix.file_descr -> socket * Unix.sockaddr >

let unix_socket transport fd : socket =
  object
    method typ = "unix"
    method file_descr = fd
    method transport = transport

    method wait_for ?log (event : event) d =
      let event =
        match event with
          | `Read -> `Read fd
          | `Write -> `Write fd
          | `Both -> `Both fd
      in
      Tutils.wait_for ?log event d

    method write = Unix.write fd
    method read = Unix.read fd
    method close = Unix.close fd
  end

let sockaddr_of_address address =
  match Unix.getaddrinfo address "0" [AI_NUMERICHOST] with
    | [] -> raise Not_found
    | addr :: _ -> addr.ai_addr

let resolve_host host port =
  match
    Unix.getaddrinfo host (string_of_int port) [AI_SOCKTYPE SOCK_STREAM]
  with
    | [] -> raise Not_found
    | l -> List.rev l

let connect_sockaddr ?bind_address ?timeout sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  (try
     match bind_address with
       | None -> ()
       | Some s -> Unix.bind socket (sockaddr_of_address s)
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     begin
       try Unix.close socket with _ -> ()
     end;
     Printexc.raise_with_backtrace exn bt);
  let do_timeout = timeout <> None in
  let check_timeout () =
    match timeout with
      | Some timeout ->
          (* Block in a select call for [timeout] seconds. *)
          let _, w, _ = Unix.select [] [socket] [] timeout in
          if w = [] then raise (Tutils.Timeout timeout);
          Unix.clear_nonblock socket;
          socket
      | None -> assert false
  in
  let finish () =
    try
      if do_timeout then Unix.set_nonblock socket;
      Unix.connect socket sockaddr;
      if do_timeout then Unix.clear_nonblock socket;
      socket
    with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _) -> check_timeout ()
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when Sys.os_type = "Win32" ->
          check_timeout ()
  in
  try finish ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    begin
      try Unix.close socket with _ -> ()
    end;
    Printexc.raise_with_backtrace e bt

let connect ?bind_address ?timeout host port =
  let rec connect_any ?bind_address ?timeout (addrs : Unix.addr_info list) =
    match addrs with
      | [] -> raise Not_found
      | [addr] ->
          (* Let a possible error bubble up *)
          connect_sockaddr ?bind_address ?timeout addr.ai_addr
      | addr :: tail -> (
          try connect_sockaddr ?bind_address ?timeout addr.ai_addr
          with _ -> connect_any ?bind_address ?timeout tail)
  in
  connect_any ?bind_address ?timeout (resolve_host host port)

let unix_transport : transport =
  object (self)
    method name = "unix"
    method protocol = "http"
    method default_port = 80

    method connect ?bind_address ?timeout host port =
      let socket = connect ?bind_address ?timeout host port in
      unix_socket self socket

    method accept fd =
      let fd, addr = Unix.accept ~cloexec:true fd in
      (unix_socket self fd, addr)
  end

let unix_socket = unix_socket unix_transport
let user_agent = Configure.vendor

let args_split s =
  let args = Hashtbl.create 2 in
  let fill_arg arg =
    match Pcre.split ~pat:"=" arg with
      | e :: l ->
          (* There should be only arg=value *)
          List.iter
            (fun v ->
              Hashtbl.replace args (Lang_string.url_decode e)
                (Lang_string.url_decode v))
            l
      | [] -> ()
  in
  List.iter fill_arg (Pcre.split ~pat:"&" s);
  args

let parse_url url =
  let basic_rex =
    Pcre.regexp "^([Hh][Tt][Tt][Pp][sS]?)://([^/:]+)(:[0-9]+)?(/.*)?$"
  in
  let sub =
    try Pcre.exec ~rex:basic_rex url
    with Not_found -> (* raise Invalid_url *)
                      failwith "Invalid URL."
  in
  let protocol = Pcre.get_substring sub 1 in
  let host = Pcre.get_substring sub 2 in
  let port =
    try
      let port = Pcre.get_substring sub 3 in
      let port = String.sub port 1 (String.length port - 1) in
      let port = int_of_string port in
      Some port
    with Not_found -> None
  in
  let path = try Pcre.get_substring sub 4 with Not_found -> "/" in
  { protocol; host; port; path }

let is_url path = Pcre.pmatch ~pat:"^[Hh][Tt][Tt][Pp][sS]?://.+" path

let dirname url =
  let rex = Pcre.regexp "^([Hh][Tt][Tt][Pp][sS]?://.+/)[^/]*$" in
  let s = Pcre.exec ~rex url in
  Pcre.get_substring s 1

(* An ugly code to read until we see [\r]?\n n times. *)
let read_crlf ?(log = fun _ -> ()) ?(max = 4096) ?(count = 2) ~timeout
    (socket : socket) =
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
    socket#wait_for ~log `Read timeout;
    let h = socket#read c 0 1 in
    if h < 1 then stop := true
    else (
      let c = Bytes.get c 0 in
      Buffer.add_char ans c;
      if c = '\n' then incr count_n else if c <> '\r' then count_n := 0);
    incr n
  done;
  Buffer.contents ans

let read ~timeout (socket : socket) len =
  if len = 0 then ""
  else (
    socket#wait_for `Read timeout;
    let buf = Bytes.create len in
    let len = socket#read buf 0 len in
    Bytes.sub_string buf 0 len)

let really_read ~timeout (socket : socket) len =
  let start_time = Unix.gettimeofday () in
  let buf = Buffer.create len in
  let rec f () =
    let now = Unix.gettimeofday () in
    let remaining = start_time +. timeout -. now in
    if remaining <= 0. then failwith "timeout!";
    socket#wait_for `Read remaining;
    let rem = len - Buffer.length buf in
    let s = Bytes.create rem in
    let n = socket#read s 0 rem in
    Buffer.add_subbytes buf s 0 n;
    if n = 0 || Buffer.length buf = len then Buffer.contents buf else f ()
  in
  f ()

(* Read chunked transfer. *)
let read_chunked ~timeout (socket : socket) =
  let read = read_crlf ~count:1 ~timeout socket in
  let len = List.hd (Pcre.split ~pat:"[\r]?\n" read) in
  let len = List.hd (Pcre.split ~pat:";" len) in
  let len = int_of_string ("0x" ^ len) in
  let s = really_read socket ~timeout len in
  ignore (read_crlf ~count:1 ~timeout socket);
  (s, len)
