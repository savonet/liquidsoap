module Pcre = Re.Pcre

let files_path = ref ""
let port = ref 8080
let domains = ref None
let usage = "usage: http [options] /path/to/files"

let () =
  let pnum = ref 0 in
  let arg s =
    incr pnum;
    if !pnum > 1 then (
      Printf.eprintf "Error: too many arguments\n";
      exit 1)
    else files_path := s
  in
  Arg.parse
    [
      ( "--domains",
        Arg.Int (fun i -> domains := Some i),
        "Number of domains serving requests. (default: one per core)" );
      ( "--port",
        Arg.Int (fun i -> port := i),
        Printf.sprintf "Port used to bind the server. (default: %d)" !port );
    ]
    arg usage;
  if !files_path = "" then (
    Printf.printf "%s\n" usage;
    exit 1)
  else ()

type priority = Maybe_blocking | Non_blocking

(* Parsing a request and writing a reply never block, so they run directly on
   a domain of the pool. Reading the file, or running a CGI, does block. *)
let scheduler =
  Duppy.create
    ~classify:(function
      | Non_blocking -> `Immediate | Maybe_blocking -> `Blocking)
    ~on_error:(fun exn _ ->
      Printf.printf "Task failed: %s\n%!" (Printexc.to_string exn))
    ()

type http_method = Post | Get
type http_protocol = Http_11 | Http_10

let string_of_protocol = function
  | Http_11 -> "HTTP/1.1"
  | Http_10 -> "HTTP/1.0"

let protocol_of_string = function
  | "HTTP/1.1" -> Http_11
  | "HTTP/1.0" -> Http_10
  | _ -> assert false

let string_of_method = function Post -> "POST" | Get -> "GET"

let method_of_string = function
  | "POST" -> Post
  | "GET" -> Get
  | _ -> assert false

type data = None | String of string | File of Unix.file_descr

type request = {
  request_protocol : http_protocol;
  request_method : http_method;
  request_uri : string;
  request_headers : (string * string) list;
  request_data : data;
}

type reply = {
  reply_protocol : http_protocol;
  reply_status : int * string;
  reply_headers : (string * string) list;
  reply_data : data;
}

exception Assoc of string

let assoc_uppercase x y =
  try
    List.iter
      (fun (l, v) ->
        if String.uppercase_ascii l = x then raise (Assoc v) else ())
      y;
    raise Not_found
  with Assoc s -> s

let server = "dhttpd"

let html_template =
  Printf.sprintf
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
     \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\r\n\
     <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\r\n\
     %s</html>"

let server_error status protocol =
  let _, explanation = status in
  let data =
    String
      (html_template
         (Printf.sprintf "<head><title>%s</title></head>\r\n<body>%s !</body>"
            explanation explanation))
  in
  {
    reply_protocol = protocol;
    reply_status = status;
    reply_headers =
      [("Content-Type", "text/html; charset=UTF-8"); ("Server", server)];
    reply_data = data;
  }

let error_404 = server_error (404, "File Not Found")
let error_500 = server_error (500, "Bad Request") Http_10
let error_403 = server_error (403, "Forbidden")

let http_302 protocol uri =
  {
    reply_protocol = protocol;
    reply_status = (302, "Found");
    reply_headers = [("Location", uri)];
    reply_data = String "";
  }

type socket_status = Keep | Close

(** A handler finishes by raising its reply, from wherever it got to. *)
exception Reply of reply

let send_reply h reply =
  let write s =
    Duppy.Io.write ~priority:Non_blocking h (Bytes.unsafe_of_string s)
  in
  let code, status = reply.reply_status in
  write
    (Printf.sprintf "%s %d %s\r\n%s\r\n\r\n"
       (string_of_protocol reply.reply_protocol)
       code status
       (String.concat "\r\n"
          (List.map
             (fun (x, y) -> Printf.sprintf "%s: %s" x y)
             reply.reply_headers)));
  match reply.reply_data with
    | String s -> write s
    | None -> ()
    | File fd ->
        let close () = try Unix.close fd with _ -> () in
        let length = 4096 in
        let buf = Bytes.create length in
        let rec copy () =
          match Unix.read fd buf 0 length with
            | 0 -> ()
            | n ->
                Duppy.Io.write ~length:n ~priority:Non_blocking h buf;
                copy ()
        in
        Fun.protect ~finally:close copy

let parse_headers headers =
  let rex = Pcre.regexp "([^:\r\n]+):\\s*([^\r\n]+)" in
  let split_header l h =
    try
      let sub = Pcre.exec ~rex h in
      (Pcre.get_substring sub 1, Pcre.get_substring sub 2) :: l
    with Not_found -> raise (Reply error_500)
  in
  List.fold_left split_header [] headers

let index_uri path index protocol uri =
  let uri =
    try
      let ret = Pcre.extract ~rex:(Pcre.regexp "([^\\?]*)\\?") uri in
      ret.(1)
    with Not_found -> uri
  in
  try
    if Sys.is_directory (Printf.sprintf "%s%s" path uri) then
      if uri.[String.length uri - 1] <> '/' then
        raise (Reply (http_302 protocol (Printf.sprintf "%s/" uri)))
      else (
        let index = Printf.sprintf "%s/%s" uri index in
        if Sys.file_exists (Printf.sprintf "%s/%s" path index) then index
        else uri)
    else uri
  with
    | Reply _ as e -> raise e
    | _ -> uri

let file_request path _ request =
  let uri =
    try
      let ret =
        Pcre.extract ~rex:(Pcre.regexp "([^\\?]*)\\?.*") request.request_uri
      in
      ret.(1)
    with Not_found -> request.request_uri
  in
  let uri = index_uri path "index.html" request.request_protocol uri in
  let fname = Printf.sprintf "%s%s" path uri in
  if not (Sys.file_exists fname) then
    raise (Reply (error_404 request.request_protocol));
  let fd =
    try Unix.openfile fname [Unix.O_RDONLY] 0o640
    with _ -> raise (Reply (error_403 request.request_protocol))
  in
  let stats = Unix.fstat fd in
  let headers =
    [("Server", server); ("Content-Length", string_of_int stats.Unix.st_size)]
  in
  let headers =
    if Pcre.pmatch ~rex:(Pcre.regexp "\\.html$") fname then
      ("Content-Type", "text/html") :: headers
    else if Pcre.pmatch ~rex:(Pcre.regexp "\\.css$") fname then
      ("Content-Type", "text/css") :: headers
    else headers
  in
  raise
    (Reply
       {
         reply_protocol = request.request_protocol;
         reply_status = (200, "OK");
         reply_headers = headers;
         reply_data = File fd;
       })

let file_handler = ((fun _ -> true), file_request !files_path)

let read_all h =
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string buf
         (Duppy.Io.read ~priority:Non_blocking h (Duppy.Io.Length 1024))
     done
   with Duppy.Io.Error _ -> ());
  Buffer.contents buf

let cgi_handler process path _ request =
  let uri, args, suffix =
    try
      let ret =
        Pcre.extract ~rex:(Pcre.regexp "([^\\?]*)\\?(.*)") request.request_uri
      in
      let args = Pcre.split ~rex:(Pcre.regexp "&") ret.(2) in
      let args =
        List.map
          (fun s ->
            let ret = Pcre.extract ~rex:(Pcre.regexp "([^=]*)=(.*)") s in
            (ret.(1), ret.(2)))
          args
      in
      (ret.(1), args, Printf.sprintf "?%s" ret.(2))
    with Not_found -> (request.request_uri, [], "")
  in
  let uri = index_uri path "index.php" request.request_protocol uri in
  let fname = Printf.sprintf "%s%s" path uri in
  if not (Sys.file_exists fname) then
    raise (Reply (error_404 request.request_protocol));
  let headers = request.request_headers in
  let env =
    Printf.sprintf
      "export GATEWAY_INTERFACE=CGI/1.1; export SERVER_SOFTWARE=%s; export \
       SERVER_PROTOCOL=%s; export REQUEST_METHOD=%s; export \
       SCRIPT_FILENAME=%s; export SCRIPT_NAME=%s; export REQUEST_URI=%s%s; \
       export QUERY_STRING=%s; export REDIRECT_STATUS=200"
      server
      (string_of_protocol request.request_protocol)
      (string_of_method request.request_method)
      (Filename.quote fname) (Filename.quote uri) (Filename.quote uri)
      (Filename.quote suffix)
      (Filename.quote
         (String.concat "&" (List.map (fun (x, y) -> x ^ "=" ^ y) args)))
  in
  let append env key =
    if List.mem_assoc key headers then
      Printf.sprintf "%s; export %s=%s" env key
        (Filename.quote (List.assoc key headers))
    else env
  in
  let env = append env "CONTENT_TYPE" in
  let env = append env "CONTENT_LENGTH" in
  let env =
    if List.mem_assoc "AUTHORIZATION" headers then (
      let ret =
        Pcre.extract
          ~rex:(Pcre.regexp "(^[^\\s]*\\s.*)$")
          (List.assoc "AUTHORIZATION" headers)
      in
      if Array.length ret > 0 then
        Printf.sprintf "%s; extract AUTH_TYPE=%s" env ret.(1)
      else raise (Reply error_500))
    else env
  in
  let env =
    List.fold_left
      (fun env (x, y) ->
        Printf.sprintf "%s; export HTTP_%s=%s" env x (Filename.quote y))
      env headers
  in
  let data =
    match request.request_data with
      | None -> ""
      | String s -> s
      | _ -> assert false
  in
  let process = Printf.sprintf "%s; %s 2>/dev/null" env process in
  let in_c, out_c = Unix.open_process process in
  let out_s = Unix.descr_of_out_channel out_c in
  let h_out = Duppy.Io.handle scheduler out_s in
  Duppy.Io.write ~priority:Non_blocking h_out (Bytes.unsafe_of_string data);
  let in_s = Unix.descr_of_in_channel in_c in
  let h_in = Duppy.Io.handle scheduler in_s in
  let headers =
    Duppy.Io.read ~priority:Non_blocking h_in (Duppy.Io.Split "[\r]?\n[\r]?\n")
  in
  let data = read_all h_in in
  ignore (Unix.close_process (in_c, out_c));
  let headers = parse_headers (Pcre.split ~rex:(Pcre.regexp "\r\n") headers) in
  let status, headers =
    if List.mem_assoc "Status" headers then (
      try
        let ans =
          Pcre.extract
            ~rex:(Pcre.regexp "([\\d]+)\\s(.*)")
            (List.assoc "Status" headers)
        in
        ( (int_of_string ans.(1), ans.(2)),
          List.filter (fun (x, _) -> x <> "Status") headers )
      with _ -> raise (Reply error_500))
    else ((200, "OK"), headers)
  in
  raise
    (Reply
       {
         reply_protocol = request.request_protocol;
         reply_status = status;
         reply_headers =
           ("Content-length", string_of_int (String.length data)) :: headers;
         reply_data = String data;
       })

let php_handler =
  ( (fun request ->
      let uri =
        index_uri !files_path "index.php" request.request_protocol
          request.request_uri
      in
      Pcre.pmatch ~rex:(Pcre.regexp "\\.php$") uri),
    cgi_handler "php-cgi" !files_path )

let handlers = [php_handler; file_handler]

let handle_request h request =
  try
    List.iter
      (fun (check, handler) -> if check request then handler h request)
      handlers;
    error_404 request.request_protocol
  with Reply reply -> reply

let parse_request h r =
  try
    let headers = Pcre.split ~rex:(Pcre.regexp "\r\n") r in
    let request, headers =
      match headers with
        | e :: l -> (e, parse_headers l)
        | _ -> raise (Reply error_500)
    in
    let http_method, uri, protocol =
      try
        let sub =
          Pcre.exec
            ~rex:(Pcre.regexp "([\\w]+)\\s([^\\s]+)\\s(HTTP/1.[01])")
            request
        in
        ( method_of_string (Pcre.get_substring sub 1),
          Pcre.get_substring sub 2,
          protocol_of_string (Pcre.get_substring sub 3) )
      with
        | Reply _ as e -> raise e
        | _ -> raise (Reply error_500)
    in
    let data =
      match http_method with
        | Get -> None
        | Post -> (
            let len =
              try int_of_string (assoc_uppercase "CONTENT-LENGTH" headers) with
                | Not_found -> 0
                | _ -> raise (Reply error_500)
            in
            match len with
              | 0 -> None
              | d ->
                  String
                    (Duppy.Io.read ~priority:Non_blocking h (Duppy.Io.Length d))
            )
    in
    {
      request_method = http_method;
      request_protocol = protocol;
      request_uri = uri;
      request_headers = headers;
      request_data = data;
    }
  with
    | Reply _ as e -> raise e
    | _ -> raise (Reply error_500)

let handle_client socket =
  let h = Duppy.Io.handle scheduler socket in
  let rec exec () =
    let keep, reply =
      try
        let data =
          Duppy.Io.read ~priority:Non_blocking h (Duppy.Io.Split "\r\n\r\n")
        in
        let request = parse_request h data in
        let reply = handle_request h request in
        let close_header headers =
          try assoc_uppercase "CONNECTION" headers = "close"
          with Not_found -> false
        in
        let keep =
          if
            request.request_protocol = Http_10
            || close_header request.request_headers
            || close_header reply.reply_headers
          then Close
          else Keep
        in
        (keep, reply)
      with
        | Reply reply -> (Close, reply)
        | Duppy.Io.Error _ -> (Close, error_500)
    in
    let keep =
      try
        send_reply h reply;
        keep
      with Duppy.Io.Error _ -> Close
    in
    if keep = Keep then exec ()
  in
  let finish () = try Unix.close socket with _ -> () in
  Duppy.run (fun () -> Fun.protect ~finally:finish exec)

let bind_addr_inet = Unix.inet_addr_of_string "0.0.0.0"
let bind_addr = Unix.ADDR_INET (bind_addr_inet, !port)
let max_conn = 100
let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

let () =
  (* See http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4640
   * for this: we want Unix EPIPE error and not SIGPIPE, which
   * crashes the program.. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe]);
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  let rec incoming _ =
    (try
       let s, _ = Unix.accept sock in
       handle_client s
     with e ->
       Printf.printf "Failed to accept new client: %S\n" (Printexc.to_string e));
    [
      {
        Duppy.Task.priority = Non_blocking;
        events = [`Read sock];
        handler = incoming;
      };
    ]
  in
  (try Unix.bind sock bind_addr
   with Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
     failwith (Printf.sprintf "port %d already taken" !port));
  Unix.listen sock max_conn;
  Duppy.Task.add scheduler
    {
      Duppy.Task.priority = Non_blocking;
      events = [`Read sock];
      handler = incoming;
    };
  Duppy.start ?domains:!domains scheduler;
  while true do
    Unix.sleep 3600
  done
