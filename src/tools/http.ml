(* Some structured exceptions *)

type error = Socket | Response | UrlDecoding
exception Error of error

let string_of_error e = 
  match e with
    | Socket -> "error while communicating to socket"
    | Response -> "invalid answer to request"
    | UrlDecoding -> "URL decoding failed"

let raise e = raise (Error e)

type connection = Unix.file_descr

(* URL encoding/decoging according to RFC 1738, RFC 1630.
 * Borrowed from ocamlnet. *)

(** Converts k to a 2-digit hexadecimal string. *)
let to_hex2 =
  let hex_digits =
    [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
       '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]
  in
    fun k ->
      let s = String.create 2 in
        s.[0] <- hex_digits.( (k lsr 4) land 15 ) ;
        s.[1] <- hex_digits.( k land 15 ) ;
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
                  String.make 1 (Char.chr ((k1 lsl 4) lor k2))
              end)
    s

let args_split s = 
  let args = Hashtbl.create 2 in
  let fill_arg arg = 
    match Pcre.split ~pat:"=" arg with
      | e :: l -> (List.iter (Hashtbl.replace args e) l) (* There should be only arg=value *)
      | [] -> ()
  in
  List.iter fill_arg (Pcre.split ~pat:"&" s) ;
  args

let http_encode url = 
  try
    (* First decode the url 
     * That way we don't reencode already encoded urls *)
    let url = url_decode url in
    let basic_rex = Pcre.regexp "^http://([^/]+)/(.*)$" in
    let path_rex = Pcre.regexp "^([^?]+)\\?(.+)$" in
    let sub = Pcre.exec ~rex:basic_rex url in
    let host,path = Pcre.get_substring sub 1,Pcre.get_substring sub 2 in
    let encode path = 
      let path = Pcre.split ~pat:"/" path in
      let path = List.map (fun x -> url_encode ~plus:false x) path in
      List.fold_left (Printf.sprintf "%s/%s") "" path
    in
    try 
      let sub = Pcre.exec ~rex:path_rex path in
      let path,options = Pcre.get_substring sub 1,Pcre.get_substring sub 2 in
      let options = args_split options in
      let args = Hashtbl.create 2 in
      Hashtbl.iter (fun a b -> Hashtbl.replace args (url_encode a) (url_encode b)) options ;
      let merge a b c = 
        match c with
          | "" -> Printf.sprintf "%s=%s" a b
          | _ -> Printf.sprintf "%s&%s=%s" c a b
      in
      let options = Hashtbl.fold merge args "" in
      let path = encode path in
      Printf.sprintf "http://%s%s?%s" host path options
    with
      | _ -> Printf.sprintf "http://%s%s" host (encode path)
  with
    | _ -> url

(** HTTP functions. *)

let connect ~timeout host port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  begin
    match timeout with
      | None -> ()
      | Some t ->
         (* Add some timeout *)
         Unix.setsockopt_float socket Unix.SO_RCVTIMEO t ;
         Unix.setsockopt_float socket Unix.SO_SNDTIMEO t 
  end ;
    try
      Unix.connect
        socket
        (Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0),port));
      socket
    with
      | e ->
          Unix.shutdown socket Unix.SHUTDOWN_ALL ;
          Unix.close socket;
          raise Socket

let disconnect socket =
  Unix.shutdown socket Unix.SHUTDOWN_ALL ;
  Unix.close socket

let read socket buflen =
  match buflen with
    | Some buflen ->
        let buf = String.create buflen in
        let n = Unix.recv socket buf 0 buflen [] in
          String.sub buf 0 n
    | None ->
        let buflen = 1024 in
        let buf = String.create buflen in
        let ans = ref "" in
        let n = ref buflen in
          while !n <> 0 do
            n := Unix.recv socket buf 0 buflen [];
            ans := !ans ^ String.sub buf 0 !n
          done;
          !ans

type status = string * int * string

let request socket request =
  if
    let len = String.length request in
      Unix.write socket request 0 len < len
  then
    raise Socket ;
  let header =
    (* We read until we see \r\n\r\n *)
    let ans = ref "" in
    let n = ref 0 in
    let loop = ref true in
    let was_n = ref false in
    let c = String.create 1 in
      (* TODO XXX What is this hard-coded 4096 ?
       *          This whole loop is ugly and unreadable... is that state
       *          machine was_n really correct ? *)
      while !loop && !n < 4096 do
        let h = Unix.read socket c 0 1 in
          if h < 1 then
            loop := false
          else
            (
              ans := !ans ^ c;
              if c = "\n" then
                (if !was_n then loop := false else was_n := true)
              else if c <> "\r" then
                was_n := false
            );
          incr n
      done;
      !ans
  in
  let header = Pcre.split ~pat:"\r\n" header in
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
               (String.lowercase !!1, !!2) :: fields
           with
             | Not_found -> fields)
        [] header
  in
    (response_http_version, response_status, response_msg), (List.rev fields)

let http_req ?(post="") ?(headers=[]) socket host port file = 
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
    Printf.sprintf "%sHost: %s:%d\r\n" req host port
  in
  let req =
    Printf.sprintf "%sUser-Agent: liquidsoap/%s (%s; ocaml %s)\r\n"
      req Configure.version Sys.os_type Sys.ocaml_version
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

let get ?(headers=[]) socket host port file =
  let req = http_req ~headers:headers socket host port file in
     request socket req


let post ?(headers=[]) data socket host port file =
  let req = http_req ~post:data ~headers:headers socket host port file in
     request socket req
