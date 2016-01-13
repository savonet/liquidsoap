(* Some structured exceptions *)

include Stdlib

type error = Socket | Response | UrlDecoding
exception Error of error

let string_of_error e =
  match e with
    | Socket -> "Http: error while communicating to socket"
    | Response -> "Http: invalid answer to request"
    | UrlDecoding -> "Http: URL decoding failed"

(** Error translator *)
let error_translator (e:exn) =
   match e with
     | Error e -> Some (string_of_error e)
     | _ -> None

let () = Utils.register_error_translator error_translator

let raise e = raise (Error e)

type connection = Unix.file_descr

let user_agent = Configure.vendor

(* URL encoding/decoding according to RFC 1738, RFC 1630.
 * Borrowed from ocamlnet. *)

(** Converts k to a 2-digit hexadecimal string. *)
let to_hex2 =
  let hex_digits =
    [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
       '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]
  in
    fun k ->
      let s = Bytes.create 2 in
        Bytes.set s 0 (hex_digits.( (k lsr 4) land 15 )) ;
        Bytes.set s 1 (hex_digits.( k land 15 )) ;
        s

let url_encode ?(plus=true) s =
  Pcre.substitute
    ~pat:"[^A-Za-z0-9_.!*-]"
    ~subst:(fun x ->
              if plus && x = " " then "+" else
                let k = Char.code x.[0] in
                  "%" ^ to_hex2 k)
    s

let of_hex1 c =
  match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _ -> raise UrlDecoding

let url_decode ?(plus = true) s =
  Pcre.substitute
    ~pat:"\\+|%..|%.|%"
      (* TODO why do we match %. and % and seem to exclude them below ? *)
    ~subst:(fun s ->
              if s = "+" then
                if plus then " " else "+"
              else begin
                (* Assertion: s.[0] = '%' *)
                if String.length s < 3 then raise UrlDecoding ;
                let k1 = of_hex1 s.[1] in
                let k2 = of_hex1 s.[2] in
                  Bytes.make 1 (Char.chr ((k1 lsl 4) lor k2))
              end)
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
  List.iter fill_arg (Pcre.split ~pat:"&" s) ;
  args

(* exception Invalid_url *)

let url_split_host_port url =
  let basic_rex = Pcre.regexp "^http://([^/:]+)(:[0-9]+)?(/.*)$" in
  let sub =
    try
      Pcre.exec ~rex:basic_rex url
    with
      | Not_found ->
        (* raise Invalid_url *)
        failwith "Invalid URL."
  in
  let host,uri = Pcre.get_substring sub 1,Pcre.get_substring sub 3 in
  let port =
    try
      let port = Pcre.get_substring sub 2 in
      let port = String.sub port 1 (String.length port - 1) in
      let port = int_of_string port in
      Some port
    with
      | Not_found -> None
  in
  host,port,uri

let is_url path =
  Pcre.pmatch ~pat:"^https?://.+" path

let dirname url =
  let rex = Pcre.regexp "^(https?://.+/)[^/]*$" in
  let s = Pcre.exec ~rex url in
  Pcre.get_substring s 1

(** HTTP functions. *)

let connect ?bind_address host port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  begin
    match bind_address with
      | None -> ()
      | Some s ->
        let bind_addr_inet = (Unix.gethostbyname s).Unix.h_addr_list.(0) in
	(* Seems like you need to bind on port 0 *)
	let bind_addr = Unix.ADDR_INET(bind_addr_inet, 0) in
	Unix.bind socket bind_addr ;
  end ;
    try
      Unix.connect
        socket
        (Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0),port));
      socket
    with
      | _ ->
          Unix.close socket;
          raise Socket

let disconnect socket =
  try
   Unix.close socket
  with
    | _ -> ()

let read ?(log=fun _ -> ()) ~timeout socket buflen =
  Tutils.wait_for ~log `Read socket timeout;
  match buflen with
    | Some buflen ->
        let buf = Bytes.create buflen in
        let n = Unix.recv socket buf 0 buflen [] in
          String.sub buf 0 n
    | None ->
        let buflen = 1024 in
        let buf = Bytes.create buflen in
        let ans = ref "" in
        let n = ref buflen in
          while !n <> 0 do
            n := Unix.recv socket buf 0 buflen [];
            ans := !ans ^ Bytes.sub buf 0 !n
          done;
          !ans

type status = string * int * string

type headers = (string*string) list

(* An ugly code to read until we see [\r]?\n n times. *)
let read_crlf ?(log=fun _ -> ()) ?(max=4096) ?(count=2) ~timeout socket =
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
      Tutils.wait_for ~log `Read socket timeout;
      let h = Unix.read socket c 0 1 in
        if h < 1 then
          stop := true
        else
          (
            Buffer.add_string ans c;
            if c = "\n" then
              incr count_n
            else if c <> "\r" then
              count_n := 0
          );
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
    assert(0 < rem);
    let s = Bytes.create rem in
    let n = Unix.read socket s 0 rem in
    Buffer.add_substring buf s 0 n;
    if Buffer.length buf = len then
      Buffer.contents buf
    else
      f ()
  in
  let s = f () in
  ignore(read_crlf ~count:1 ~timeout socket);
  s, len

let request ?(log=fun _ -> ()) ~timeout socket request =
  if
    let len = String.length request in
      Tutils.wait_for ~log `Write socket timeout;
      Unix.write socket request 0 len < len
  then
    raise Socket ;
  let header = read_crlf ~log ~timeout socket in
  let header = Pcre.split ~pat:"[\r]?\n" header in
  let response,header =
     match header with
      | e::tl -> e,tl
      | [] -> raise Response
  in
  let response_http_version, response_status, response_msg =
    let pat = "^((?:HTTP/[0-9.]+)|ICY) ([0-9]+) (.*)$" in
      try
        let (!!) = Pcre.get_substring (Pcre.exec ~pat response) in
          !!1, int_of_string !!2, !!3
      with
        | Not_found -> raise Response
  in
  let fields =
    let pat = "([^:]*):\\s*(.*)" in
      List.fold_left
        (fun fields line ->
           try
             let (!!) = Pcre.get_substring (Pcre.exec ~pat line) in
               (Utils.StringCompat.lowercase_ascii !!1, !!2) :: fields
           with
             | Not_found -> fields)
        [] header
  in
    (response_http_version, response_status, response_msg), (List.rev fields)

let http_req ?(post="") ?(headers=[]) host port file =
  let action =
    if post <> "" then
      "POST"
    else
      "GET"
  in
  let req =
    Printf.sprintf "%s %s HTTP/1.0\r\n" action file
  in
  let req =
    if port = 80 then 
      Printf.sprintf "%sHost: %s\r\n" req host
    else
      Printf.sprintf "%sHost: %s:%d\r\n" req host port
  in
  let req =
    if not (List.mem_assoc "User-Agent" headers) then
      Printf.sprintf "%sUser-Agent: %s\r\n"
        req user_agent
    else
      req
  in
  let req =
    List.fold_left
      (fun s (t,v) -> Printf.sprintf "%s%s: %s\r\n" s t v)
      req headers
  in
  if post <> "" then
    Printf.sprintf
      "%sContent-Length: %d\r\n\r\n%s\r\n"
      req (String.length post)  post
  else
    Printf.sprintf "%s\r\n" req

let get ?(headers=[]) ?log ~timeout socket host port file =
  let req = http_req ~headers:headers host port file in
     request ?log ~timeout socket req

let post ?(headers=[]) ?log ~timeout data socket host port file =
  let req = http_req ~post:data ~headers:headers host port file in
     request ?log ~timeout socket req

type request = Get | Post of string

let full_request ?headers ?(port=80) ?(log=fun _ -> ()) 
                 ~timeout ~host ~url ~request () =
 let connection =
   connect host port
 in
 Tutils.finalize ~k:(fun () -> Unix.close connection)
  (fun () ->
    (* We raise an error if the statuses are not correct. *)
    let status,headers =
      match request with
        | Get ->
           get ?headers ~log ~timeout connection host port url
        | Post data ->
           post ?headers ~log ~timeout data connection host port url
    in
    let max =
      try
        let (_,len) = List.find (fun (l,_) ->
          Utils.StringCompat.lowercase_ascii l = "content-length")
          headers
        in
        int_of_string len
      with _ -> max_int
    in
    let ret =
      read_crlf ~log ~timeout ~max connection
    in
    status,headers,
       Pcre.substitute
          ~pat:"[\r]?\n$" ~subst:(fun _ -> "") ret)
