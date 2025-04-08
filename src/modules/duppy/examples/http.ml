module Pcre = Re.Pcre

let non_blocking_queues = ref 3
let maybe_blocking_queues = ref 1
let files_path = ref ""
let port = ref 8080
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
      ( "--non_blocking_queues",
        Arg.Int (fun i -> non_blocking_queues := i),
        Printf.sprintf "Number of non-blocking queues. (default: %d)"
          !non_blocking_queues );
      ( "--maybe_blocking_queues",
        Arg.Int (fun i -> maybe_blocking_queues := i),
        Printf.sprintf "Number of maybe-blocking queues. (default: %d)"
          !maybe_blocking_queues );
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

let scheduler = Duppy.create ()

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

let send_reply h reply =
  let write s =
    Duppy.Monad.Io.write ?timeout:None ~priority:Non_blocking h
      (Bytes.unsafe_of_string s)
  in
  let code, status = reply.reply_status in
  let http_header =
    Printf.sprintf "%s %d %s\r\n%s\r\n\r\n"
      (string_of_protocol reply.reply_protocol)
      code status
      (String.concat "\r\n"
         (List.map
            (fun (x, y) -> Printf.sprintf "%s: %s" x y)
            reply.reply_headers))
  in
  Duppy.Monad.bind (write http_header) (fun () ->
      match reply.reply_data with
        | String s -> write s
        | File fd ->
            let stats = Unix.fstat fd in
            let ba =
              Unix.map_file fd Bigarray.char Bigarray.c_layout false
                [| stats.Unix.st_size |]
            in
            let ba = Bigarray.array1_of_genarray ba in
            let close () = try Unix.close fd with _ -> () in
            let on_error e =
              close ();
              h.Duppy.Monad.Io.on_error e
            in
            let h = { h with Duppy.Monad.Io.on_error } in
            Duppy.Monad.bind
              (Duppy.Monad.Io.write_bigarray ?timeout:None
                 ~priority:Non_blocking h ba) (fun () ->
                Duppy.Monad.return (close ()))
        | None -> Duppy.Monad.return ())

let parse_headers headers =
  let split_header l h =
    try
      let rex = Pcre.regexp "([^:\\r\\n]+):\\s*([^\\r\\n]+)" in
      let sub = Pcre.exec ~rex h in
      Duppy.Monad.return
        ((Pcre.get_substring sub 1, Pcre.get_substring sub 2) :: l)
    with Not_found -> Duppy.Monad.raise error_500
  in
  Duppy.Monad.fold_left split_header [] headers

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
        Duppy.Monad.raise (http_302 protocol (Printf.sprintf "%s/" uri))
      else (
        let index = Printf.sprintf "%s/%s" uri index in
        if Sys.file_exists (Printf.sprintf "%s/%s" path index) then
          Duppy.Monad.return index
        else Duppy.Monad.return uri)
    else Duppy.Monad.return uri
  with _ -> Duppy.Monad.return uri

let file_request path _ request =
  let uri =
    try
      let ret =
        Pcre.extract ~rex:(Pcre.regexp "([^\\?]*)\\?.*") request.request_uri
      in
      ret.(1)
    with Not_found -> request.request_uri
  in
  let __pa_duppy_0 = index_uri path "index.html" request.request_protocol uri in
  Duppy.Monad.bind __pa_duppy_0 (fun uri ->
      let fname = Printf.sprintf "%s%s" path uri in
      if Sys.file_exists fname then (
        try
          let fd = Unix.openfile fname [Unix.O_RDONLY] 0o640 in
          let stats = Unix.fstat fd in
          let headers =
            [
              ("Server", server);
              ("Content-Length", string_of_int stats.Unix.st_size);
            ]
          in
          let headers =
            if Pcre.pmatch ~rex:(Pcre.regexp "\\.html$") fname then
              ("Content-Type", "text/html") :: headers
            else if Pcre.pmatch ~rex:(Pcre.regexp "\\.css$") fname then
              ("Content-Type", "text/css") :: headers
            else headers
          in
          Duppy.Monad.raise
            {
              reply_protocol = request.request_protocol;
              reply_status = (200, "OK");
              reply_headers = headers;
              reply_data = File fd;
            }
        with _ -> Duppy.Monad.raise (error_403 request.request_protocol))
      else Duppy.Monad.raise (error_404 request.request_protocol))

let file_handler = ((fun _ -> Duppy.Monad.return true), file_request !files_path)

let cgi_handler process path h request =
  let uri, args, suffix =
    try
      let ret =
        Pcre.extract ~rex:(Pcre.regexp "([^\\?]*)\\?(.*)") request.request_uri
      in
      try
        let ans =
          Pcre.extract ~rex:(Pcre.regexp "^([^/]*)/([^&=]*)$") ret.(2)
        in
        (ret.(1), ans.(1), ans.(2))
      with Not_found -> (ret.(1), ret.(2), "")
    with Not_found -> (request.request_uri, "", "")
  in
  let __pa_duppy_0 = index_uri path "index.php" request.request_protocol uri in
  Duppy.Monad.bind __pa_duppy_0 (fun script ->
      let script = Printf.sprintf "%s%s" path script in
      let env =
        Printf.sprintf
          "export SERVER_SOFTWARE=Duppy-httpd/1.0; export \
           SERVER_NAME=localhost; export GATEWAY_INTERFACE=CGI/1.1; export \
           SERVER_PROTOCOL=%s; export SERVER_PORT=%d; export \
           REQUEST_METHOD=%s; export REQUEST_URI=%s; export \
           REDIRECT_STATUS=200; export SCRIPT_FILENAME=%s"
          (string_of_protocol request.request_protocol)
          !port
          (string_of_method request.request_method)
          (Filename.quote uri) (Filename.quote script)
      in
      let env =
        Printf.sprintf "%s; export QUERY_STRING=%s" env (Filename.quote args)
      in
      let env =
        let tr_suffix = Printf.sprintf "%s%s" path suffix in
        (* Trick ! *)
        let tr_suffix =
          Printf.sprintf "%s/%s"
            (Filename.dirname tr_suffix)
            (Filename.basename tr_suffix)
        in
        Printf.sprintf "%s; export PATH_TRANSLATED=%s; export PATH_INFO=%s" env
          (Filename.quote tr_suffix) (Filename.quote suffix)
      in
      let sanitize s =
        Pcre.substitute ~rex:(Pcre.regexp "-")
          ~subst:(fun _ -> "_")
          (String.uppercase_ascii s)
      in
      let headers =
        List.map (fun (x, y) -> (sanitize x, y)) request.request_headers
      in
      let append env key =
        if List.mem_assoc key headers then
          Printf.sprintf "%s; export %s=%s" env key
            (Filename.quote (List.assoc key headers))
        else env
      in
      let env = append env "CONTENT_TYPE" in
      let env = append env "CONTENT_LENGTH" in
      let __pa_duppy_0 =
        if List.mem_assoc "AUTHORIZATION" headers then (
          let ret =
            Pcre.extract
              ~rex:(Pcre.regexp "(^[^\\s]*\\s.*)$")
              (List.assoc "AUTHORIZATION" headers)
          in
          if Array.length ret > 0 then
            Duppy.Monad.return
              (Printf.sprintf "%s; extract AUTH_TYPE=%s" env ret.(1))
          else Duppy.Monad.raise error_500)
        else Duppy.Monad.return env
      in
      Duppy.Monad.bind __pa_duppy_0 (fun env ->
          let f env (x, y) =
            Printf.sprintf "%s; export HTTP_%s=%s" env x (Filename.quote y)
          in
          let env = List.fold_left f env headers in
          let data =
            match request.request_data with
              | None -> ""
              | String s -> s
              | _ -> assert false
          in
          (* not implemented *)
          let process = Printf.sprintf "%s; %s 2>/dev/null" env process in
          let in_c, out_c = Unix.open_process process in
          let out_s = Unix.descr_of_out_channel out_c in
          let h = { h with Duppy.Monad.Io.socket = out_s; data = "" } in
          let __pa_duppy_0 =
            Duppy.Monad.Io.write ?timeout:None ~priority:Non_blocking h
              (Bytes.unsafe_of_string data)
          in
          Duppy.Monad.bind __pa_duppy_0 (fun () ->
              let in_s = Unix.descr_of_in_channel in_c in
              let h = { h with Duppy.Monad.Io.socket = in_s; data = "" } in
              let __pa_duppy_0 =
                Duppy.Monad.Io.read ?timeout:None ~priority:Non_blocking
                  ~marker:(Duppy.Io.Split "[\r]?\n[\r]?\n") h
              in
              Duppy.Monad.bind __pa_duppy_0 (fun headers ->
                  let __pa_duppy_0 =
                    Duppy.Monad.catch
                      (Duppy.Monad.Io.read_all ?timeout:None
                         ~priority:Non_blocking h.Duppy.Monad.Io.scheduler in_s)
                      (fun (s, _) -> Duppy.Monad.return s)
                  in
                  Duppy.Monad.bind __pa_duppy_0 (fun data ->
                      let data =
                        Printf.sprintf "%s%s" h.Duppy.Monad.Io.data data
                      in
                      ignore (Unix.close_process (in_c, out_c));
                      let __pa_duppy_0 =
                        let headers =
                          Pcre.split ~rex:(Pcre.regexp "\r\n") headers
                        in
                        parse_headers headers
                      in
                      Duppy.Monad.bind __pa_duppy_0 (fun headers ->
                          let __pa_duppy_0 =
                            if List.mem_assoc "Status" headers then (
                              try
                                let ans =
                                  Pcre.extract
                                    ~rex:(Pcre.regexp "([\\d]+)\\s(.*)")
                                    (List.assoc "Status" headers)
                                in
                                Duppy.Monad.return
                                  ( (int_of_string ans.(1), ans.(2)),
                                    List.filter
                                      (fun (x, _) -> x <> "Status")
                                      headers )
                              with _ -> Duppy.Monad.raise error_500)
                            else Duppy.Monad.return ((200, "OK"), headers)
                          in
                          Duppy.Monad.bind __pa_duppy_0
                            (fun (status, headers) ->
                              let headers =
                                ( "Content-length",
                                  string_of_int (String.length data) )
                                :: headers
                              in
                              Duppy.Monad.raise
                                {
                                  reply_protocol = request.request_protocol;
                                  reply_status = status;
                                  reply_headers = headers;
                                  reply_data = String data;
                                })))))))

let php_handler =
  ( (fun request ->
      let __pa_duppy_0 =
        index_uri !files_path "index.php" request.request_protocol
          request.request_uri
      in
      Duppy.Monad.bind __pa_duppy_0 (fun uri ->
          Duppy.Monad.return (Pcre.pmatch ~rex:(Pcre.regexp "\\.php$") uri))),
    cgi_handler "php-cgi" !files_path )

let handlers = [php_handler; file_handler]

let handle_request h request =
  let f (check, handler) =
    let __pa_duppy_0 = check request in
    Duppy.Monad.bind __pa_duppy_0 (fun check ->
        if check then handler h request else Duppy.Monad.return ())
  in
  Duppy.Monad.catch
    (Duppy.Monad.bind (Duppy.Monad.iter f handlers) (fun () ->
         Duppy.Monad.return (error_404 request.request_protocol)))
    (fun reply -> Duppy.Monad.return reply)

let parse_request h r =
  try
    let headers = Pcre.split ~rex:(Pcre.regexp "\r\n") r in
    let __pa_duppy_0 =
      match headers with
        | e :: l ->
            let __pa_duppy_0 = parse_headers l in
            Duppy.Monad.bind __pa_duppy_0 (fun headers ->
                Duppy.Monad.return (e, headers))
        | _ -> Duppy.Monad.raise error_500
    in
    Duppy.Monad.bind __pa_duppy_0 (fun (request, headers) ->
        let rex = Pcre.regexp "([\\w]+)\\s([^\\s]+)\\s(HTTP/1.[01])" in
        let __pa_duppy_0 =
          try
            let sub = Pcre.exec ~rex request in
            let http_method, uri, protocol =
              ( Pcre.get_substring sub 1,
                Pcre.get_substring sub 2,
                Pcre.get_substring sub 3 )
            in
            Duppy.Monad.return
              (method_of_string http_method, uri, protocol_of_string protocol)
          with _ -> Duppy.Monad.raise error_500
        in
        Duppy.Monad.bind __pa_duppy_0 (fun (http_method, uri, protocol) ->
            let __pa_duppy_0 =
              match http_method with
                | Get -> Duppy.Monad.return None
                | Post ->
                    let __pa_duppy_0 =
                      try
                        let length = assoc_uppercase "CONTENT-LENGTH" headers in
                        Duppy.Monad.return (int_of_string length)
                      with
                        | Not_found -> Duppy.Monad.return 0
                        | _ -> Duppy.Monad.raise error_500
                    in
                    Duppy.Monad.bind __pa_duppy_0 (fun len ->
                        match len with
                          | 0 -> Duppy.Monad.return None
                          | d ->
                              let __pa_duppy_0 =
                                Duppy.Monad.Io.read ?timeout:None
                                  ~priority:Non_blocking
                                  ~marker:(Duppy.Io.Length d) h
                              in
                              Duppy.Monad.bind __pa_duppy_0 (fun data ->
                                  Duppy.Monad.return (String data)))
            in
            Duppy.Monad.bind __pa_duppy_0 (fun data ->
                Duppy.Monad.return
                  {
                    request_method = http_method;
                    request_protocol = protocol;
                    request_uri = uri;
                    request_headers = headers;
                    request_data = data;
                  })))
  with _ -> Duppy.Monad.raise error_500

let handle_client socket =
  (* Read and process lines *)
  let on_error _ = error_500 in
  let h = { Duppy.Monad.Io.scheduler; socket; data = ""; on_error } in
  let rec exec () =
    let __pa_duppy_0 =
      Duppy.Monad.catch
        (let __pa_duppy_0 =
           Duppy.Monad.Io.read ?timeout:None ~priority:Non_blocking
             ~marker:(Duppy.Io.Split "\r\n\r\n") h
         in
         Duppy.Monad.bind __pa_duppy_0 (fun data ->
             let __pa_duppy_0 = parse_request h data in
             Duppy.Monad.bind __pa_duppy_0 (fun request ->
                 let __pa_duppy_0 = handle_request h request in
                 Duppy.Monad.bind __pa_duppy_0 (fun reply ->
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
                     Duppy.Monad.return (keep, reply)))))
        (fun reply -> Duppy.Monad.return (Close, reply))
    in
    Duppy.Monad.bind __pa_duppy_0 (fun (keep, reply) ->
        Duppy.Monad.bind (send_reply h reply) (fun () ->
            if keep = Keep then exec () else Duppy.Monad.return ()))
  in
  let finish _ = try Unix.close socket with _ -> () in
  Duppy.Monad.run ~return:finish ~raise:finish (exec ())

let new_queue ~priority ~name () =
  let priorities p = p = priority in
  let queue () = Duppy.queue scheduler ~log:(fun _ -> ()) ~priorities name in
  Thread.create queue ()

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
  for i = 1 to !non_blocking_queues do
    ignore
      (new_queue ~priority:Non_blocking
         ~name:(Printf.sprintf "Non blocking queue #%d" i)
         ())
  done;
  for i = 1 to !maybe_blocking_queues do
    ignore
      (new_queue ~priority:Maybe_blocking
         ~name:(Printf.sprintf "Maybe blocking queue #%d" i)
         ())
  done;
  Duppy.queue scheduler ~log:(fun _ -> ()) "root"
