type uri = {
  protocol : string;
  host : string;
  port : int option;
  path : string;
}

type event = Cry.event

type socket =
  < typ : string
  ; transport : transport
  ; file_descr : Unix.file_descr
  ; wait_for : ?log:(string -> unit) -> event -> float -> unit
  ; write : Bytes.t -> int -> int -> int
  ; read : Bytes.t -> int -> int -> int
  ; close : unit >

and server =
  < transport : transport
  ; accept : ?timeout:float -> Unix.file_descr -> socket * Unix.sockaddr >

and transport =
  < name : string
  ; protocol : string
  ; default_port : int
  ; connect :
      ?bind_address:string ->
      ?timeout:float ->
      ?prefer:[ `System_default | `Ipv4 | `Ipv6 ] ->
      string ->
      int ->
      socket
  ; server : server >

let connect = Cry.unix_connect

let rec accept ?timeout sock =
  let has_timeout = timeout <> None in
  let check_timeout () =
    Tutils.wait_for (`Read sock) (Option.get timeout);
    match Unix.getsockopt_error sock with
      | Some err -> raise (Unix.Unix_error (err, "accept", ""))
      | None ->
          Unix.clear_nonblock sock;
          accept ?timeout sock
  in
  try
    if has_timeout then Unix.set_nonblock sock;
    let fd, addr = Unix.accept ~cloexec:true sock in
    if has_timeout then (
      Unix.clear_nonblock sock;
      Unix.clear_nonblock fd);
    (fd, addr)
  with
    | Unix.Unix_error (Unix.EAGAIN, _, _) when has_timeout -> check_timeout ()
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when has_timeout ->
        check_timeout ()

let rec unix_socket fd =
  let s = Cry.unix_socket fd in
  object
    method typ = s#typ
    method file_descr = fd
    method transport = unix_transport ()
    method wait_for = s#wait_for
    method write = s#write
    method read = s#read
    method close = s#close
  end

and unix_transport () =
  object (self)
    method name = Cry.unix_transport#name
    method protocol = Cry.unix_transport#protocol
    method default_port = Cry.unix_transport#default_port

    method connect ?bind_address ?timeout ?prefer host port =
      let fd = Cry.unix_connect ?bind_address ?timeout ?prefer host port in
      unix_socket fd

    method server =
      object
        method transport = self

        method accept ?timeout fd =
          let fd, addr = accept ?timeout fd in
          (unix_socket fd, addr)
      end
  end

let unix_transport = unix_transport ()
let user_agent = Configure.vendor

let args_split s =
  let args = Hashtbl.create 2 in
  let fill_arg arg =
    match Re.Pcre.split ~rex:(Re.Pcre.regexp "=") arg with
      | e :: l ->
          (* There should be only arg=value *)
          List.iter
            (fun v ->
              Hashtbl.replace args (Lang_string.url_decode e)
                (Lang_string.url_decode v))
            l
      | [] -> ()
  in
  List.iter fill_arg (Re.Pcre.split ~rex:(Re.Pcre.regexp "&") s);
  args

let parse_url url =
  let basic_rex =
    Re.Pcre.regexp "^([Hh][Tt][Tt][Pp][sS]?)://([^/:]+)(:[0-9]+)?(/.*)?$"
  in
  let sub =
    try Re.Pcre.exec ~rex:basic_rex url
    with Not_found -> (* raise Invalid_url *)
                      failwith "Invalid URL."
  in
  let protocol = Re.Pcre.get_substring sub 1 in
  let host = Re.Pcre.get_substring sub 2 in
  let port =
    try
      let port = Re.Pcre.get_substring sub 3 in
      let port = String.sub port 1 (String.length port - 1) in
      let port = int_of_string port in
      Some port
    with Not_found -> None
  in
  let path = try Re.Pcre.get_substring sub 4 with Not_found -> "/" in
  { protocol; host; port; path }

let is_url path =
  Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "^[Hh][Tt][Tt][Pp][sS]?://.+") path

let dirname url =
  let rex = Re.Pcre.regexp "^([Hh][Tt][Tt][Pp][sS]?://.+/)[^/]*$" in
  let s = Re.Pcre.exec ~rex url in
  Re.Pcre.get_substring s 1

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
  let len = List.hd (Re.Pcre.split ~rex:(Re.Pcre.regexp "[\r]?\n") read) in
  let len = List.hd (Re.Pcre.split ~rex:(Re.Pcre.regexp ";") len) in
  let len = int_of_string ("0x" ^ len) in
  let s = really_read socket ~timeout len in
  ignore (read_crlf ~count:1 ~timeout socket);
  (s, len)

let set_socket_default ~read_timeout ~write_timeout fd =
  Unix.set_close_on_exec fd;
  Unix.setsockopt_float fd Unix.SO_RCVTIMEO read_timeout;
  Unix.setsockopt_float fd Unix.SO_SNDTIMEO write_timeout

type auth = { user : string; password : string }

let parse_auth s =
  match Re.Pcre.split ~rex:(Re.Pcre.regexp ":") s with
    | user :: password :: rem ->
        { user; password = String.concat ":" (password :: rem) }
    | _ -> raise Not_found
