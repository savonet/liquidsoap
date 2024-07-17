(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Unix
open Dtools
open Http_source

let conf_harbor =
  Conf.void ~p:(Configure.conf#plug "harbor")
    "Builtin HTTP stream receiver."
let conf_harbor_port =
  Conf.int ~p:(conf_harbor#plug "port") ~d:8005
    "Port on which the HTTP stream receiver should listen."
let conf_harbor_bind_addr =
  Conf.string ~p:(conf_harbor#plug "bind_addr") ~d:"0.0.0.0"
    "IP address on which the HTTP stream receiver should listen."
let conf_harbor_user =
  Conf.string ~p:(conf_harbor#plug "username") ~d:"source"
    "Default username for source connection."
let conf_harbor_pass =
  Conf.string ~p:(conf_harbor#plug "password") ~d:"hackme"
    "Default password for source connection."
let conf_icy =
  Conf.bool ~p:(conf_harbor#plug "icy") ~d:false
      "Enable the builtin ICY (shout) stream receiver."
let conf_timeout =
  Conf.float ~p:(conf_harbor#plug "timeout") ~d:30.
        "Timeout for source connections."

let log = Log.make ["harbor"]

exception Internal

(* Define what we need as a source *)
class virtual source =
object(self)
  inherit Source.source

  method virtual relay : Unix.file_descr -> unit
  method virtual insert_metadata : (string, string) Hashtbl.t -> unit
  method virtual login : (string option)*(string option)
  method virtual is_taken : bool
  method virtual register_decoder : string -> unit
  method virtual get_type : string option

end

let sources : (string,source) Hashtbl.t = Hashtbl.create 1

(* Add your sources *)
let add_source mountpoint source =
  log#f 3 "Adding mountpoint '%s' to list..." mountpoint ;
  Hashtbl.add sources mountpoint source

(* ... maybe remove them *)
let remove_source mountpoint =
  Hashtbl.remove sources mountpoint

let find_source mountpoint =
  Hashtbl.find sources mountpoint

(** {1 Handling of a client} *)

exception Exit
exception Too_many_sources
exception Not_authenticated
exception Xaudiocast_auth
(* Answer to close communication *)
exception Answer of (unit->unit)
exception Not_supported
exception Unknown_codec
exception Mount_taken

type request_type =
  | Source
  | Get
  | Shout
  | Invalid of string (* Used for icy when wrong password *)
  | Unhandled

type protocol =
  | Http_10
  | Http_11
  | Icy
  | Unknown of string (* Used for xaudiocast *)

let http_error_page code status msg =
  ( "HTTP/1.0 " ^ (string_of_int code) ^ " " ^ status ^ "\r\n\
     Content-Type: text/html\r\n\r\n\
     <?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
     <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
     \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n\
     <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\
     <head><title>Liquidsoap source harbor</title></head>\
     <body><p>" ^ msg ^ "</p></body></html>\n" )

let parse_icy_request_line r =
      (match r with
         | s when s = conf_harbor_pass#get -> Shout
         | s -> Invalid(s) ),
      "/",
      Icy

let parse_http_request_line r =
  let data = Str.split (Str.regexp "[ \t]+") r in
    (
      (match (String.uppercase (List.nth data 0)) with
        | "SOURCE" -> Source
        | "GET" -> Get
        | _ -> Unhandled),
      (List.nth data 1),
      (match (String.uppercase (List.nth data 2)) with
        | "HTTP/1.0" -> Http_10
        | "HTTP/1.1" -> Http_11
        | s -> Unknown(s))
    )

let write_answer ?(keep=false) c a =
  ignore (Unix.write c a 0 (String.length a)) ;
  if not keep then
    try
      Unix.shutdown c Unix.SHUTDOWN_ALL ;
      Unix.close c
    with
      | _ -> ()

let parse_headers headers =
  let split_header h l =
    try
      let rex = Pcre.regexp "([^:\\r\\n]+):\\s*([^\\r\\n]+)" in
      let sub = Pcre.exec ~rex h in
      (String.uppercase (Pcre.get_substring sub 1),
       Pcre.get_substring sub 2) :: l
    with
      | Not_found -> l
  in
  let headers = List.fold_right split_header headers [] in
  List.iter (fun (h, v) -> log#f 4 "Header: %s, value: %s." h v) headers ;
  headers

let auth_check ~login c uri headers =
    (* 401 error model *)
    let answer s =
      write_answer c
        (http_error_page 401
           "Unauthorized\r\n\
            WWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
           s)
    in
    let valid_username,valid_password =
      let f g x = match x with Some y -> y | None -> g in
      match login with
        | x,y ->
           f conf_harbor_user#get x,
           f conf_harbor_pass#get y
    in
    try
      (* Authentication *)
      let auth = List.assoc "AUTHORIZATION" headers in
      let data = Str.split (Str.regexp "[ \t]+") auth in
        if List.nth data 0 <> "Basic" then raise Not_supported ;
        let auth_data =
          Str.split (Str.regexp ":") (Utils.decode64 (List.nth data 1))
        in
        let user,pass = List.nth auth_data 0, List.nth auth_data 1 in
          log#f 4 "Requested username: %s, password: %s." user pass ;
          log#f 4
            "Valid username: %s, password: %s."
            valid_username valid_password ;
          if user <> valid_username || pass <> valid_password then
            raise Not_authenticated ;
          (* OK *)
          log#f 4 "Client logged in."
    with
      | Not_found ->
          if uri = valid_password then
            ( log#f 4 "xaudiocast login" ;
            raise Xaudiocast_auth )
          else
            raise (Answer(fun () ->
                ( log#f 3 "Returned 401: no authentification given." ;
                  answer "No login / password supplied." ) ) )
      | Not_authenticated ->
            raise (Answer(fun () ->
             ( log#f 3 "Returned 401: wrong auth." ;
               answer "Wrong Authentification data") ) )
      | Not_supported ->
            raise (Answer(fun () ->
             ( log#f 3 "Returned 401: bad authentification." ;
               answer "No login / password supplied.") ) )

let handle_source_request ~icy hprotocol c uri headers =
  try
    let s = find_source uri in
    let icy,uri =
      try
        (* ICY auth check was done before.. *)
        if not icy then
          auth_check ~login:s#login c uri headers ;
        icy,uri
      with
        | Xaudiocast_auth ->
            begin match hprotocol with
                    | Unknown(s) ->
                        write_answer ~keep:true c "OK\r\n\r\n" ;
                        true,s
                    | _ ->
                       failwith
                         "Incorrect xaudiocast source request."
            end
        | e -> raise e
    in
    let sproto = match icy with
                  | true -> "ICY"
                  | false -> "SOURCE"
    in
    log#f 3 "%s request on %s." sproto uri ;
    begin
      match s#login,icy with
        | (_,Some x),true ->
           log#f 2 "ICY protocol should not be used on a mountpoint \
                    with custom password. Metadata update will not \
                    work."

        | _ -> ()
    end ;
    let stype =
      try
        List.assoc "CONTENT-TYPE" headers
      with
        | Not_found when icy -> "audio/mpeg"
        | Not_found -> raise Unknown_codec
    in
    match s#is_taken with
      | true -> raise Mount_taken
      | _ ->
          s#register_decoder stype ;
          log#f 3 "Adding source on mountpoint '%s' with type '%s'." uri stype ;
          if not icy then write_answer ~keep:true c "HTTP/1.0 200 OK\r\n\r\n" ;
          s#relay c
  with
    | Mount_taken ->
        log#f 3 "Returned 401: Mount taken" ;
        write_answer c
          (http_error_page 401
             "Unauthorized\r\n\
              WWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
             "Mountpoint in use") ;
        failwith "Mountpoint in use"
    | Not_found ->
        log#f 3 "Returned 404 for '%s'." uri ;
        write_answer c
          (http_error_page 404 "Not found"
             "This mountpoint isn't available.") ;
        failwith "no such mountpoint"
    | Unknown_codec ->
        log#f 3 "Returned 401: unknown audio codec" ;
        write_answer c
          (http_error_page 401 "Not recognized"
             "This stream's format recognized.") ;
        failwith "bad codec"
    | Answer s ->
          s () ;
          failwith "wrong source authentification"
    | e ->
        log#f 3 "Returned 401 for '%s'." uri ;
        write_answer c
          (http_error_page 401 "Error"
             "The server could not handle your request.") ;
        failwith (Printexc.to_string e)

let handle_get_request c uri headers =
  let default =
    "HTTP/1.0 200 OK\r\n\
     Content-Type: text/html\r\n\r\n\
     <?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
     <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
     \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n\
     <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\
     <head><title>Liquidsoap source harbor</title></head>\
     <body><p>Liquidsoap's harbor main page</p></body></html>\n"
  in
  let ans_404 = fun () ->
    log#f 3 "Returned 404 for '%s'." uri ;
    write_answer c (http_error_page 404 "Not found"
    "This page isn't available.")
  in
  let ans_401 = fun () ->
    log#f 3 "Returned 401 for '%s'." uri ;
    write_answer c (http_error_page 401 "Unknown request"
    "Unknown request.")
  in
  let admin args =
    match
      try Hashtbl.find args "mode"
      with Not_found -> raise (Answer(ans_401))
    with
      | "updinfo" ->
          let mount =
            try
              Hashtbl.find args "mount"
            with Not_found -> "/"
          in
            log#f 3 "Request to update metadata for mount %s" mount;
            let s = find_source mount in
              begin try
                auth_check ~login:s#login c uri headers
              with
                | e ->
                    begin try
                      let pass =
                        match snd s#login with
                          | Some y -> y
                          | None -> conf_harbor_pass#get
                      in
                      let ans () =
                        log#f 3 "Returned 401 for '%s': wrong auth." uri ;
                        write_answer c
                          (http_error_page 401 "Authentification Failed"
                             "Wrong Authentification data")
                      in
                        if Hashtbl.find args "pass" <> pass then
                          raise (Answer ans)
                    with
                      | Not_found -> raise e
                    end ;
                    let ans () =
                      log#f 3 "Returned 401 for '%s': Source is not mp3." uri ;
                      write_answer c
                        (http_error_page 401 "Unknown request"
                           "Source is not mp3")
                    in
                      if s#get_type <> Some "audio/mpeg" then
                        raise (Answer ans) ;
                      let ans =
                        Printf.sprintf
                          "HTTP/1.0 200 OK\r\n\r\n\
                           Updated metadatas for mount %s\n"
                          mount
                      in
                        Hashtbl.remove args "mount";
                        Hashtbl.remove args "mode";
                        s#insert_metadata args ;
                        raise (Answer (fun () -> write_answer c ans))
              end
     | _ -> raise (Answer ans_401)
  in
  let rex = Pcre.regexp "^(.+)\\?(.+)$" in
  let base_uri,args =
  try
    let sub = Pcre.exec ~rex:rex uri in
    Pcre.get_substring sub 1,
    Pcre.get_substring sub 2
  with
    | Not_found -> uri,""
  in
  log#f 3 "GET request on %s." base_uri ;
  let args = Http.args_split (Http.url_decode args) in
  Hashtbl.iter (fun h v -> log#f 4 "GET Arg: %s, value: %s." h v) args ;
  try
     match base_uri with
       | "/" -> write_answer c default
       | "/admin/metadata" | "/admin.cgi"
             -> admin args
       | _ -> raise (Answer(ans_404))
  with
    | Answer(s) ->  s ()
    | e -> ans_404 () ; failwith (Printexc.to_string e)

let priority = Tutils.Non_blocking

let handle_client ~icy socket =
  let on_error _ =
    log#f 3 "Client left." ;
    try
      Unix.shutdown socket Unix.SHUTDOWN_ALL ;
      Unix.close socket
    with
      | _ -> ()
  in
  (* Read and process lines *)
  let marker =
    match icy with
      | true -> Duppy.Io.Split "[\r]?\n"
      | false -> Duppy.Io.Split "[\r]?\n[\r]?\n"
  in
  let recursive = false in
  let parse = match icy with
                 | true -> parse_icy_request_line
                 | false -> parse_http_request_line
  in
  let process l =
    let l =
      match List.rev l with
        | []
        | _ :: [] -> assert false (* Should not happen *)
        | e :: l -> List.rev l
    in
    try
      let s =
        match l with
          | s :: _ -> s
          | _ -> failwith "could not parse client handshake."
      in
      let lines = Str.split (Str.regexp "\n") s in
      let (hmethod, huri, hprotocol) = parse (List.nth lines 0) in
      let headers = parse_headers (List.tl lines) in
        match hmethod with
          | Source when not icy ->
              handle_source_request ~icy hprotocol socket huri headers
          | Get when not icy ->
              handle_get_request socket huri headers
          | Shout when icy ->
              write_answer ~keep:true socket "OK\r\n\r\n" ;
              handle_source_request ~icy:true hprotocol socket huri headers
          | Invalid s ->
              let er = if icy then "ICY " else "" in
              log#f 3 "Invalid %srequest" er ;
              write_answer socket (Printf.sprintf "%s\r\n" s) ;
              failwith (Printf.sprintf "Invalid %srequest" er)
          | _ ->
            log#f 3 "Returned 501." ;
            write_answer socket
              (http_error_page 501 "Not Implemented"
                 "The server did not understand your request.") ;
            failwith "cannot handle this, exiting"
    with
      | Failure s -> log#f 3 "Failed: %s" s
    in
      Duppy.Io.read ~priority ~recursive ~on_error
        Tutils.scheduler socket marker process

(* {1 The server} *)

let start_harbor () =
  let rec incoming ~icy sock _ =
    begin
      try
        let (socket,caller) = accept sock in
        let ip =
          let a = match caller with
                    | ADDR_INET (a,_) -> a
                    | _ -> assert false
          in
          try
            (gethostbyaddr a).h_name
          with
            | Not_found -> string_of_inet_addr a
        in
        (* Add timeout *)
        Unix.setsockopt_float socket Unix.SO_RCVTIMEO conf_timeout#get ;
        Unix.setsockopt_float socket Unix.SO_SNDTIMEO conf_timeout#get ;
        handle_client icy socket ;
        log#f 3 "New client: %s" ip
      with e -> log#f 2 "Failed to accept new client: %S" (Printexc.to_string e)
    end ;
    [{ Duppy.Task.
         priority = priority ;
         events = [`Read sock] ;
         handler = (incoming ~icy sock) }]
  in
  let open_socket port =
    let bind_addr = conf_harbor_bind_addr#get in
    let bind_addr_inet =
      inet_addr_of_string bind_addr
    in
    let bind_addr = ADDR_INET(bind_addr_inet, port) in
    let max_conn = Hashtbl.length sources in
    let sock = socket PF_INET SOCK_STREAM 0 in
    setsockopt sock SO_REUSEADDR true ;
    (* Set TCP_NODELAY on the socket *)
    Liq_sockets.set_tcp_nodelay sock true ;
    (* Add timeout *)
    Unix.setsockopt_float sock Unix.SO_RCVTIMEO conf_timeout#get ;
    Unix.setsockopt_float sock Unix.SO_SNDTIMEO conf_timeout#get ;
    begin try bind sock bind_addr with
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
          failwith (Printf.sprintf "port %d already taken" port)
    end ;
    listen sock max_conn ;
    sock
  in
  let port = conf_harbor_port#get in
  let sock = open_socket port in
  Duppy.Task.add Tutils.scheduler
    { Duppy.Task.
        priority = priority ;
        events   = [`Read sock] ;
        handler  = incoming ~icy:false sock } ;
  (* Now do the same for ICY if enabled *)
  if conf_icy#get then
    (* Open port+1 *)
    let port = port+1 in
    let sock = open_socket port in
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = priority ;
          events   = [`Read sock] ;
          handler  = incoming ~icy:true sock }

let start () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  if Hashtbl.length sources > 0 then begin
    Tutils.need_non_blocking_queue () ;
    start_harbor ()
  end

let () = ignore (Dtools.Init.at_start start)
