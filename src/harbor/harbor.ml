(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2016 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

*****************************************************************************)
open Harbor_base
module Monad = Duppy.Monad

module type Monad_t = module type of Monad with module Io := Monad.Io

module type Transport_t = sig
  type socket

  val name : string
  val file_descr_of_socket : socket -> Unix.file_descr
  val read : socket -> bytes -> int -> int -> int
  val accept : Unix.file_descr -> socket * Unix.sockaddr
  val close : socket -> unit

  module Duppy : sig
    module Io : Duppy.Io_t with type socket = socket

    module Monad : sig
      module Io :
        Duppy.Monad.Monad_io_t with type socket = socket and module Io = Io

      include Monad_t
    end
  end

  module Http : Http.Http_t with type connection = socket
  module Websocket : Websocket.Websocket_t with type socket = socket
end

module Unix_transport = struct
  type socket = Unix.file_descr

  let name = "unix"
  let file_descr_of_socket socket = socket
  let read = Unix.read
  let accept fd = Unix.accept fd
  let close = Unix.close

  module Duppy = Duppy
  module Http = Http
  module Websocket = Websocket
end

module type T = sig
  type socket

  exception Retry
  exception Assoc of string
  exception Not_authenticated
  exception Unknown_codec
  exception Mount_taken
  exception Registered
  exception Websocket_closed

  (* Generic *)

  val file_descr_of_socket : socket -> Unix.file_descr
  val read : socket -> bytes -> int -> int -> int
  val close : socket -> unit

  (* Http Server *)

  type http_verb = [ `Get | `Post | `Put | `Delete | `Head | `Options ]
  type reply = Close of (unit -> string) | Relay of string * (unit -> unit)

  type http_handler =
    protocol:string ->
    data:string ->
    headers:(string * string) list ->
    socket:socket ->
    string ->
    (reply, reply) Duppy.Monad.t

  val verb_of_string : string -> http_verb
  val mk_simple : string -> unit -> string
  val simple_reply : string -> ('a, reply) Duppy.Monad.t
  val reply : (unit -> string) -> ('a, reply) Duppy.Monad.t

  val add_http_handler :
    port:int -> verb:http_verb -> uri:string -> http_handler -> unit

  val remove_http_handler :
    port:int -> verb:http_verb -> uri:string -> unit -> unit

  (* Source input *)

  class virtual source :
    kind:Frame.content_kind
    -> object
         inherit Source.source

         method virtual relay :
           string ->
           (string * string) list ->
           ?read:(socket -> bytes -> int -> int -> int) ->
           socket ->
           unit

         method virtual insert_metadata : (string, string) Hashtbl.t -> unit

         method virtual login : string * (string -> string -> bool)

         method virtual icy_charset : string option

         method virtual meta_charset : string option

         method virtual get_mime_type : string option
       end

  val http_auth_check :
    ?args:(string, string) Hashtbl.t ->
    login:string * (string -> string -> bool) ->
    (string * string) list ->
    (unit, reply) Duppy.Monad.t

  val relayed : string -> (unit -> unit) -> ('a, reply) Duppy.Monad.t
  val add_source : port:int -> mountpoint:string -> icy:bool -> source -> unit
  val remove_source : port:int -> mountpoint:string -> unit -> unit
end

module Make (T : Transport_t) : T with type socket = T.socket = struct
  module Http = T.Http
  module Websocket = T.Websocket
  module Task = Duppy.Task
  module Duppy = T.Duppy

  type socket = T.socket

  let file_descr_of_socket = T.file_descr_of_socket
  let read = T.read
  let close = T.close

  let protocol_name =
    match T.name with
      | "unix" -> "HTTP"
      | "ssl" -> "HTTPS"
      | "secure_transport" -> "HTTPS"
      | _ -> assert false

  (* Define what we need as a source *)

  (** Raised when source needs to retry read. *)
  exception Retry

  class virtual source ~kind =
    object
      inherit Source.source ~name:"input.harbor" kind

      method virtual relay
          : string ->
            (string * string) list ->
            ?read:(socket -> bytes -> int -> int -> int) ->
            socket ->
            unit

      method virtual insert_metadata : (string, string) Hashtbl.t -> unit

      method virtual login : string * (string -> string -> bool)

      method virtual icy_charset : string option

      method virtual meta_charset : string option

      method virtual get_mime_type : string option
    end

  type sources = (string, source) Hashtbl.t
  type http_verb = [ `Get | `Post | `Put | `Delete | `Head | `Options ]
  type source_type = [ `Put | `Post | `Source | `Xaudiocast | `Shout ]

  type verb =
    [ `Get | `Post | `Put | `Delete | `Head | `Options | `Source | `Shout ]

  let verb_of_string s =
    match String.uppercase_ascii s with
      | "GET" -> `Get
      | "POST" -> `Post
      | "PUT" -> `Put
      | "DELETE" -> `Delete
      | "HEAD" -> `Head
      | "OPTIONS" -> `Options
      | _ -> raise Not_found

  let verb_or_source_of_string s =
    match String.uppercase_ascii s with
      | "SOURCE" -> `Source
      | _ -> verb_of_string s

  let string_of_verb = function
    | `Get -> "GET"
    | `Post -> "POST"
    | `Put -> "PUT"
    | `Delete -> "DELETE"
    | `Head -> "HEAD"
    | `Options -> "OPTIONS"
    | _ -> assert false

  type protocol =
    [ `Http_10
    | `Http_11
    | `Ice_10
    | `Icy
    | `Xaudiocast_uri of string
    | `Websocket ]

  let string_of_protocol : protocol -> string = function
    | `Http_10 -> "HTTP/1.0"
    | `Http_11 -> "HTTP/1.1"
    | `Ice_10 -> "ICE/1.0"
    | `Icy -> "ICY"
    | `Xaudiocast_uri uri -> Printf.sprintf "X-AUDIOCAST (%s)" uri
    | `Websocket -> "WEBSOCKET"

  type reply = Close of (unit -> string) | Relay of string * (unit -> unit)

  let mk_simple s =
    let ret = ref s in
    fun () ->
      let r = !ret in
      ret := "";
      r

  let simple_reply s = Duppy.Monad.raise (Close (mk_simple s))
  let reply s = Duppy.Monad.raise (Close s)
  let relayed s f = Duppy.Monad.raise (Relay (s, f))

  type http_handler =
    protocol:string ->
    data:string ->
    headers:(string * string) list ->
    socket:socket ->
    string ->
    (reply, reply) Duppy.Monad.t

  type http_handlers = (http_verb * string, http_handler) Hashtbl.t
  type handler = { sources : sources; http : http_handlers }
  type open_port = handler * Unix.file_descr list

  let opened_ports : (int, open_port) Hashtbl.t = Hashtbl.create 1
  let find_handler = Hashtbl.find opened_ports

  let find_source mount port =
    Hashtbl.find (fst (find_handler port)).sources mount

  exception Assoc of string

  let assoc_uppercase x y =
    try
      List.iter
        (fun (l, v) ->
          if String.uppercase_ascii l = x then raise (Assoc v) else ())
        y;
      raise Not_found
    with Assoc s -> s

  exception Not_authenticated
  exception Unknown_codec
  exception Mount_taken
  exception Registered

  let http_error_page code status msg =
    "HTTP/1.0 " ^ string_of_int code ^ " " ^ status
    ^ "\r\n\
       Content-Type: text/html\r\n\
       \r\n\
       <?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
       <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
       \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n\
       <html xmlns=\"http://www.w3.org/1999/xhtml\" \
       xml:lang=\"en\"><head><title>Liquidsoap source \
       harbor</title></head><body><p>" ^ msg ^ "</p></body></html>"

  (* The error numbers are specified here:
     http://tools.ietf.org/html/rfc6455#section-7.4.1 *)
  let websocket_error n msg = Websocket.to_string (`Close (Some (n, msg)))

  let parse_icy_request_line ~port h r =
    let auth_data = Pcre.split ~pat:":" r in
    let requested_user, password =
      match auth_data with
        | user :: password :: _ -> (user, password)
        | _ -> ("", r)
    in
    let __pa_duppy_0 =
      try Duppy.Monad.return (find_source "/" (port - 1))
      with Not_found ->
        log#info "ICY error: no / mountpoint";
        simple_reply "No / mountpoint\r\n\r\n"
    in
    Duppy.Monad.bind __pa_duppy_0 (fun s ->
        (* Authentication can be blocking. *)
        Duppy.Monad.Io.exec ~priority:Tutils.Maybe_blocking h
          (let user, auth_f = s#login in
           let user = if requested_user = "" then user else requested_user in
           if auth_f user password then Duppy.Monad.return (`Shout, "/", `Icy)
           else (
             log#info "ICY error: invalid password";
             simple_reply "Invalid password\r\n\r\n" )))

  let parse_http_request_line r =
    try
      let data = Pcre.split ~rex:(Pcre.regexp "[ \t]+") r in
      let protocol = verb_or_source_of_string (List.nth data 0) in
      Duppy.Monad.return
        ( protocol,
          List.nth data 1,
          match String.uppercase_ascii (List.nth data 2) with
            | "HTTP/1.0" -> `Http_10
            | "HTTP/1.1" -> `Http_11
            | "ICE/1.0" -> `Ice_10
            | s when protocol = `Source -> `Xaudiocast_uri s
            | _ -> raise Not_found )
    with e ->
      log#info "Invalid request line %s: %s" r (Printexc.to_string e);
      simple_reply "HTTP 500 Invalid request\r\n\r\n"

  let parse_headers headers =
    let split_header h l =
      try
        let rex = Pcre.regexp "([^:\\r\\n]+):\\s*([^\\r\\n]+)" in
        let sub = Pcre.exec ~rex h in
        (Pcre.get_substring sub 1, Pcre.get_substring sub 2) :: l
      with Not_found -> l
    in
    let f x = String.uppercase_ascii x in
    let headers = List.fold_right split_header headers [] in
    let display_headers =
      List.filter
        (fun (x, _) -> conf_pass_verbose#get || f x <> "AUTHORIZATION")
        headers
    in
    List.iter
      (fun (h, v) -> log#info "Header: %s, value: %s." h v)
      display_headers;
    headers

  let auth_check ~auth_f user pass =
    (* OK *)
    if conf_pass_verbose#get then
      log#info "Requested username: %s, password: %s." user pass
    else ();
    if not (auth_f user pass) then raise Not_authenticated else ();
    log#info "Client logged in.";
    Duppy.Monad.return ()

  let http_auth_check ?args ~login headers =
    (* 401 error model *)
    let http_reply s =
      simple_reply
        (http_error_page 401
           "Unauthorized\r\nWWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
           s)
    in
    let valid_user, auth_f = login in
    try
      let user, pass =
        try
          (* HTTP authentication *)
          let auth = assoc_uppercase "AUTHORIZATION" headers in
          let data = Pcre.split ~rex:(Pcre.regexp "[ \t]+") auth in
          match data with
            | "Basic" :: x :: _ -> (
                let auth_data = Pcre.split ~pat:":" (Utils.decode64 x) in
                match auth_data with
                  | x :: y :: _ -> (x, y)
                  | _ -> raise Not_found )
            | _ -> raise Not_found
        with Not_found -> (
          match args with
            | Some args ->
                (* ICY updates are done with
                         * password sent in GET args
                         * and user being valid_user
                         * or user, if given. *)
                let user =
                  try Hashtbl.find args "user" with Not_found -> valid_user
                in
                (user, Hashtbl.find args "pass")
            | _ -> raise Not_found )
      in
      auth_check ~auth_f user pass
    with
      | Not_authenticated ->
          log#info "Returned 401: wrong auth.";
          http_reply "Wrong Authentication data"
      | Not_found ->
          log#info "Returned 401: bad authentication.";
          http_reply "No login / password supplied."

  let exec_http_auth_check ?args ~login h headers =
    Duppy.Monad.Io.exec ~priority:Tutils.Maybe_blocking h
      (http_auth_check ?args ~login headers)

  let handle_source_request ~port ~auth ~smethod hprotocol h uri headers =
    (* ICY request are on port+1 *)
    let source_port = if smethod = `Shout then port - 1 else port in
    let __pa_duppy_0 =
      try Duppy.Monad.return (find_source uri source_port)
      with Not_found ->
        log#info "Request failed: no mountpoint '%s'!" uri;
        simple_reply
          (http_error_page 404 "Not found" "This mountpoint isn't available.")
    in
    Duppy.Monad.bind __pa_duppy_0 (fun s ->
        Duppy.Monad.bind
          ( if (* ICY and Xaudiocast auth check was done before.. *)
               not auth then exec_http_auth_check ~login:s#login h headers
          else Duppy.Monad.return () )
          (fun () ->
            try
              let sproto =
                match (smethod : source_type) with
                  | `Shout -> "ICY"
                  | `Source -> "SOURCE"
                  | `Put -> "PUT (source)"
                  | `Post -> "POST (source)"
                  | `Xaudiocast -> "X-AUDIOCAST"
              in
              log#info "%s request on %s." sproto uri;
              let stype =
                try assoc_uppercase "CONTENT-TYPE" headers with
                  | Not_found when smethod = `Shout || smethod = `Xaudiocast ->
                      "audio/mpeg"
                  | Not_found -> raise Unknown_codec
              in
              let chunked =
                try assoc_uppercase "TRANSFER-ENCODING" headers = "chunked"
                with Not_found -> false
              in
              let read =
                if chunked then (
                  let buf = Buffer.create Utils.pagesize in
                  let read connection b ofs len =
                    if Buffer.length buf < len then (
                      let s, len =
                        Http.read_chunked ~timeout:conf_timeout#get connection
                      in
                      Buffer.add_substring buf s 0 len );
                    let len = min len (Buffer.length buf) in
                    Buffer.blit buf 0 b ofs len;
                    Utils.buffer_drop buf len;
                    len
                  in
                  Some read )
                else None
              in
              let f () = s#relay ?read stype headers h.Duppy.Monad.Io.socket in
              log#info "Adding source on mountpoint %S with type %S." uri stype;
              log#debug "Relaying %s." (string_of_protocol hprotocol);
              let protocol =
                match hprotocol with
                  | `Icy -> "ICY"
                  | `Ice_10 | `Http_10 -> "HTTP/1.0"
                  | `Http_11 -> "HTTP/1.1"
                  | _ -> assert false
              in
              relayed (Printf.sprintf "%s 200 OK\r\n\r\n" protocol) f
            with
              | Mount_taken ->
                  log#info "Returned 403: Mount taken";
                  simple_reply
                    (http_error_page 403
                       "Mountpoint already taken\r\n\
                        WWW-Authenticate: Basic realm=\"Liquidsoap harbor\""
                       "Mountpoint in use")
              | Not_found ->
                  log#info "Returned 404 for '%s'." uri;
                  simple_reply
                    (http_error_page 404 "Not found"
                       "This mountpoint isn't available.")
              | Unknown_codec ->
                  log#info "Returned 501: unknown audio codec";
                  simple_reply
                    (http_error_page 501 "Not Implemented"
                       "This stream's format is not recognized.")
              | e ->
                  log#info "Returned 500 for '%s': %s" uri
                    (Printexc.to_string e);
                  simple_reply
                    (http_error_page 500 "Internal Server Error"
                       "The server could not handle your request.")))

  exception Websocket_closed

  let handle_websocket_request ~port h mount headers =
    let json_string_of = function `String s -> s | _ -> raise Not_found in
    let extract_packet s =
      let json =
        match Configure.JSON.from_string s with
          | `Assoc json -> json
          | _ -> raise Not_found
      in
      let packet_type =
        match List.assoc "type" json with
          | `String s -> s
          | _ -> raise Not_found
      in
      let data =
        match List.assoc "data" json with `Assoc data -> Some data | _ -> None
      in
      (packet_type, data)
    in
    let read_hello s =
      let error () = simple_reply (websocket_error 1002 "Invalid hello.") in
      try
        match Websocket.read s with
          | `Text s -> (
              log#debug "Hello packet: %s\n%!" s;
              match extract_packet s with
                | "hello", data ->
                    let data = Utils.get_some data in
                    let mime = json_string_of (List.assoc "mime" data) in
                    let user = json_string_of (List.assoc "user" data) in
                    let password =
                      json_string_of (List.assoc "password" data)
                    in
                    Duppy.Monad.return (mime, mount, user, password)
                | _ -> error () )
          | _ -> error ()
      with _ -> error ()
    in
    Duppy.Monad.bind
      (Duppy.Monad.Io.write ?timeout:(Some conf_timeout#get)
         ~priority:Tutils.Non_blocking h
         (Bytes.of_string (Websocket.upgrade headers)))
      (fun () ->
        let __pa_duppy_0 =
          Duppy.Monad.Io.exec ~priority:Tutils.Blocking h
            (read_hello h.Duppy.Monad.Io.socket)
        in
        Duppy.Monad.bind __pa_duppy_0 (fun (stype, huri, user, password) ->
            log#info "Mime type: %s" stype;
            log#info "Mount point: %s" huri;
            let __pa_duppy_0 =
              try Duppy.Monad.return (find_source huri port)
              with Not_found ->
                log#info "Request failed: no mountpoint '%s'!" huri;
                simple_reply
                  (websocket_error 1011 "This mountpoint isn't available.")
            in
            Duppy.Monad.bind __pa_duppy_0 (fun source ->
                let _, auth_f = source#login in
                let __pa_duppy_0 =
                  try auth_check ~auth_f user password
                  with Not_authenticated ->
                    log#info "Authentication failed!";
                    simple_reply (websocket_error 1011 "Authentication failed.")
                in
                Duppy.Monad.bind __pa_duppy_0 (fun () ->
                    let binary_data = Buffer.create Utils.pagesize in
                    let read_socket socket =
                      match Websocket.read socket with
                        | `Binary buf -> Buffer.add_string binary_data buf
                        | `Text s -> (
                            match extract_packet s with
                              | "metadata", data ->
                                  log#debug "Metadata packet: %s\n%!" s;
                                  let data = Utils.get_some data in
                                  let m =
                                    List.map
                                      (fun (l, v) -> (l, json_string_of v))
                                      data
                                  in
                                  let m =
                                    let ans = Hashtbl.create (List.length m) in
                                    (* TODO: convert charset *)
                                    let g x = x in
                                    List.iter
                                      (fun (l, v) ->
                                        Hashtbl.add ans (g l) (g v))
                                      m;
                                    ans
                                  in
                                  source#insert_metadata m;
                                  raise Retry
                              | _ -> raise Retry )
                        | `Close _ -> raise Websocket_closed
                        | _ -> raise Retry
                    in
                    let read socket buf ofs len =
                      if Buffer.length binary_data = 0 then read_socket socket
                      else ();
                      let len = min (Buffer.length binary_data) len in
                      Buffer.blit binary_data 0 buf ofs len;
                      Utils.buffer_drop binary_data len;
                      len
                    in
                    let f () =
                      source#relay stype headers ~read h.Duppy.Monad.Io.socket
                    in
                    relayed "" f))))

  exception Handled of http_handler

  let handle_http_request ~hmethod ~hprotocol ~data ~port h uri headers =
    let ans_404 () =
      log#info "Returned 404 for '%s'." uri;
      simple_reply
        (http_error_page 404 "Not found" "This page isn't available.")
    in
    let ans_500 () =
      log#info "Returned 500 for '%s'." uri;
      simple_reply
        (http_error_page 500 "Internal Server Error"
           "There was an error processing your request.")
    in
    let ans_401 () =
      log#info "Returned 401 for '%s': wrong auth." uri;
      simple_reply
        (http_error_page 401 "Authentication Failed"
           "Wrong Authentication data")
    in
    let admin ~icy args =
      let __pa_duppy_0 =
        try Duppy.Monad.return (Hashtbl.find args "mode")
        with Not_found -> ans_401 ()
      in
      Duppy.Monad.bind __pa_duppy_0 (fun mode ->
          match mode with
            | "updinfo" ->
                let mount =
                  try Hashtbl.find args "mount" with Not_found -> "/"
                in
                log#info "Request to update metadata for mount %s on port %i"
                  mount port;
                let __pa_duppy_0 =
                  try Duppy.Monad.return (find_source mount port)
                  with Not_found ->
                    log#info
                      "Returned 401 for '%s': No mountpoint '%s' on port %d."
                      uri mount port;
                    simple_reply
                      (http_error_page 401 "Request Failed"
                         "No such mountpoint")
                in
                Duppy.Monad.bind __pa_duppy_0 (fun s ->
                    Duppy.Monad.bind
                      (exec_http_auth_check ~args ~login:s#login h headers)
                      (fun () ->
                        Duppy.Monad.bind
                          ( if
                            not
                              (List.mem
                                 (Utils.get_some s#get_mime_type)
                                 conf_icy_metadata#get)
                          then (
                            log#info
                              "Returned 405 for '%s': Source format does not \
                               support ICY metadata update"
                              uri;
                            simple_reply
                              (http_error_page 405 "Method Not Allowed"
                                 "Method Not Allowed") )
                          else Duppy.Monad.return () )
                          (fun () ->
                            Hashtbl.remove args "mount";
                            Hashtbl.remove args "mode";
                            let in_enc =
                              try
                                let enc =
                                  match
                                    String.uppercase_ascii
                                      (Hashtbl.find args "charset")
                                  with
                                    | "LATIN1" -> "ISO-8859-1"
                                    | s -> s
                                in
                                Hashtbl.remove args "charset";
                                Some enc
                              with Not_found ->
                                if icy then s#icy_charset else s#meta_charset
                            in
                            (* Recode tags.. *)
                            let f x y m =
                              let g = Configure.recode_tag ?in_enc in
                              Hashtbl.add m (g x) (g y);
                              m
                            in
                            let args =
                              Hashtbl.fold f args
                                (Hashtbl.create (Hashtbl.length args))
                            in
                            s#insert_metadata args;
                            simple_reply
                              (Printf.sprintf
                                 "HTTP/1.0 200 OK\r\n\
                                  \r\n\
                                  Updated metadatas for mount %s" mount))))
            | _ -> ans_500 ())
    in
    let rex = Pcre.regexp "^(.+)\\?(.+)$" in
    let base_uri, args =
      try
        let sub = Pcre.exec ~rex uri in
        (Pcre.get_substring sub 1, Pcre.get_substring sub 2)
      with Not_found -> (uri, "")
    in
    let smethod = string_of_verb hmethod in
    let protocol =
      match hprotocol with
        | `Http_10 -> "HTTP/1.0"
        | `Http_11 -> "HTTP/1.1"
        | _ -> assert false
    in
    log#info "%s %s request on %s." protocol_name smethod base_uri;
    let args = Http.args_split args in
    (* Filter out password *)
    let log_args =
      if conf_pass_verbose#get then args
      else (
        let log_args = Hashtbl.copy args in
        Hashtbl.remove log_args "pass";
        log_args )
    in
    Hashtbl.iter (log#info "%s Arg: %s, value: %s." protocol_name) log_args;

    (* First, try with a registered handler. *)
    let handler, _ = find_handler port in
    let f (verb, reg_uri) handler =
      let rex = Pcre.regexp reg_uri in
      if (verb :> verb) = hmethod && Pcre.pmatch ~rex uri then (
        log#info "Found handler '%s %s' on port %d." smethod reg_uri port;
        raise (Handled handler) )
      else ()
    in
    try
      Hashtbl.iter f handler.http;

      (* Otherwise, try with a standard handler. *)
      match base_uri with
        (* Icecast *)
        | "/admin/metadata" -> admin ~icy:false args
        (* Shoutcast *)
        | "/admin.cgi" -> admin ~icy:true args
        | _ -> ans_404 ()
    with
      | Handled handler ->
          Duppy.Monad.Io.exec ~priority:Tutils.Maybe_blocking h
            (handler ~protocol ~data ~headers ~socket:h.Duppy.Monad.Io.socket
               uri)
      | e ->
          log#info "%s %s request on uri '%s' failed: %s" protocol_name smethod
            (Printexc.to_string e) uri;
          ans_500 ()

  let handle_client ~port ~icy h =
    (* Read and process lines *)
    let __pa_duppy_0 =
      Duppy.Monad.Io.read ?timeout:(Some conf_timeout#get)
        ~priority:Tutils.Non_blocking
        ~marker:
          ( match icy with
            | true -> Duppy.Io.Split "[\r]?\n"
            | false -> Duppy.Io.Split "[\r]?\n[\r]?\n" )
        h
    in
    Duppy.Monad.bind __pa_duppy_0 (fun s ->
        let lines = Pcre.split ~rex:(Pcre.regexp "[\r]?\n") s in
        let headers = parse_headers (List.tl lines) in
        let __pa_duppy_0 =
          let s = List.hd lines in
          if icy then parse_icy_request_line ~port h s
          else parse_http_request_line s
        in
        Duppy.Monad.bind __pa_duppy_0 (fun (hmethod, huri, hprotocol) ->
            let hprotocol =
              try
                if
                  hmethod = `Get
                  && assoc_uppercase "UPGRADE" headers <> "websocket"
                then raise Exit
                else ();
                if assoc_uppercase "SEC-WEBSOCKET-PROTOCOL" headers <> "webcast"
                then raise Exit
                else ();
                `Websocket
              with _ -> hprotocol
            in
            let is_source =
              try
                ignore (find_source huri port);
                true
              with Not_found -> false
            in
            let handle_source smethod =
              let __pa_duppy_0 =
                (* X-audiocast sends lines of the form:
         * [SOURCE password path] *)
                match hprotocol with
                  | `Xaudiocast_uri uri ->
                      let password = huri in
                      (* We check authentication here *)
                      let __pa_duppy_0 =
                        try Duppy.Monad.return (find_source uri port)
                        with Not_found ->
                          log#info "Request failed: no mountpoint '%s'!" uri;
                          simple_reply
                            (http_error_page 404 "Not found"
                               "This mountpoint isn't available.")
                      in
                      Duppy.Monad.bind __pa_duppy_0 (fun s ->
                          (* Authentication can be blocking *)
                          Duppy.Monad.Io.exec
                            ~priority:
                              (* ICY = true means that authentication has already
                   * hapenned *)
                              Tutils.Maybe_blocking h
                            (let valid_user, auth_f = s#login in
                             if not (auth_f valid_user password) then
                               simple_reply "Invalid password!"
                             else Duppy.Monad.return (true, uri, `Xaudiocast)))
                  | _ -> Duppy.Monad.return (false, huri, smethod)
              in
              Duppy.Monad.bind __pa_duppy_0 (fun (auth, huri, smethod) ->
                  handle_source_request ~port ~auth ~smethod hprotocol h huri
                    headers)
            in
            match hmethod with
              | `Put when is_source -> handle_source `Put
              | `Post when is_source -> handle_source `Post
              | `Source when not icy -> handle_source `Source
              | `Get when hprotocol = `Websocket ->
                  handle_websocket_request ~port h huri headers
              | (`Get | `Post | `Put | `Delete | `Options | `Head) when not icy
                ->
                  let len =
                    try int_of_string (assoc_uppercase "CONTENT-LENGTH" headers)
                    with _ -> 0
                  in
                  let __pa_duppy_0 =
                    if len > 0 then
                      Duppy.Monad.Io.read ?timeout:(Some conf_timeout#get)
                        ~priority:Tutils.Non_blocking
                        ~marker:(Duppy.Io.Length len) h
                    else Duppy.Monad.return ""
                  in
                  Duppy.Monad.bind __pa_duppy_0 (fun data ->
                      handle_http_request ~hmethod ~hprotocol ~data ~port h huri
                        headers)
              | `Shout when icy ->
                  Duppy.Monad.bind
                    (Duppy.Monad.Io.write ?timeout:(Some conf_timeout#get)
                       ~priority:Tutils.Non_blocking h
                       (Bytes.of_string "OK2\r\nicy-caps:11\r\n\r\n"))
                    (fun () ->
                      (* Now parsing headers *)
                      let __pa_duppy_0 =
                        Duppy.Monad.Io.read ?timeout:(Some conf_timeout#get)
                          ~priority:Tutils.Non_blocking
                          ~marker:(Duppy.Io.Split "[\r]?\n[\r]?\n") h
                      in
                      Duppy.Monad.bind __pa_duppy_0 (fun s ->
                          let lines =
                            Pcre.split ~rex:(Pcre.regexp "[\r]?\n") s
                          in
                          let headers = parse_headers lines in
                          handle_source_request ~port ~auth:true ~smethod:`Shout
                            hprotocol h huri headers))
              | _ ->
                  log#info "Returned 501: not implemented";
                  simple_reply
                    (http_error_page 501 "Not Implemented"
                       "The server did not understand your request.")))

  (* {1 The server} *)
  (* Open a port and listen to it. *)
  let open_port ~icy port =
    log#info "Opening port %d with icy = %b" port icy;
    let max_conn = conf_harbor_max_conn#get in
    let process_client sock =
      try
        let socket, caller = T.accept sock in
        let ip = Utils.name_of_sockaddr ~rev_dns:conf_revdns#get caller in
        log#info "New client on port %i: %s" port ip;
        let unix_socket = T.file_descr_of_socket socket in
        Unix.setsockopt unix_socket Unix.TCP_NODELAY true;
        let on_error e =
          ( match e with
            | Duppy.Io.Io_error -> log#info "Client %s disconnected" ip
            | Duppy.Io.Timeout ->
                log#info "Timeout while communicating with client %s." ip
            | Duppy.Io.Unix (c, p, m) ->
                log#info "%s" (Printexc.to_string (Unix.Unix_error (c, p, m)))
            | Duppy.Io.Unknown e -> log#info "%s" (Printexc.to_string e) );

          (* Sending an HTTP response in case of timeout
           * even though ICY connections are not HTTP.. *)
          if e = Duppy.Io.Timeout then
            Close
              (mk_simple
                 (http_error_page 408 "Request Time-out"
                    "The server timed out waiting for the request."))
          else Close (mk_simple "")
        in
        let h =
          {
            Duppy.Monad.Io.scheduler = Tutils.scheduler;
            socket;
            data = "";
            on_error;
          }
        in
        let rec reply r =
          let close () = try close socket with _ -> () in
          let s, exec =
            match r with
              | Relay (s, exec) -> (s, exec)
              | Close fn ->
                  let s = fn () in
                  let exec =
                    if s = "" then close else fun () -> reply (Close fn)
                  in
                  (s, exec)
          in
          let on_error e =
            ignore (on_error e);
            close ()
          in
          Duppy.Io.write ~timeout:conf_timeout#get ~priority:Tutils.Non_blocking
            ~on_error ~string:(Bytes.of_string s) ~exec Tutils.scheduler socket
        in
        Duppy.Monad.run ~return:reply ~raise:reply (handle_client ~port ~icy h)
      with e ->
        log#severe "Failed to accept new client: %s" (Printexc.to_string e)
    in
    let rec incoming ~port ~icy events out_s e =
      if List.mem (`Read out_s) e then (
        try
          List.iter
            (function `Read s -> Unix.close s | _ -> assert false)
            events;
          []
        with _ -> [] )
      else (
        let get_sock = function `Read sock -> sock | _ -> assert false in
        List.iter process_client (List.map get_sock e);
        [
          {
            Task.priority = Tutils.Non_blocking;
            events;
            handler = incoming ~port ~icy events out_s;
          };
        ] )
    in
    let open_socket port bind_addr =
      let bind_addr_inet = Unix.inet_addr_of_string bind_addr in
      let bind_addr = Unix.ADDR_INET (bind_addr_inet, port) in
      let domain = Unix.domain_of_sockaddr bind_addr in
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
      (* Set TCP_NODELAY on the socket *)
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.setsockopt sock Unix.TCP_NODELAY true;
      Unix.bind sock bind_addr;
      Unix.listen sock max_conn;
      `Read sock
    in
    let bind_addrs =
      List.fold_left
        (fun cur bind_addr -> if bind_addr <> "" then bind_addr :: cur else cur)
        [] conf_harbor_bind_addrs#get
    in
    let in_s, out_s = Unix.pipe () in
    let events = `Read in_s :: List.map (open_socket port) bind_addrs in
    Task.add Tutils.scheduler
      {
        Task.priority = Tutils.Non_blocking;
        events;
        handler = incoming ~port ~icy events in_s;
      };
    out_s

  (* This, contrary to the find_xx functions
   * creates the handlers when they are missing. *)
  let get_handler ~icy port =
    try
      let h, socks = Hashtbl.find opened_ports port in
      (* If we have only one socket and icy=true,
       * we need to open a second one. *)
      if List.length socks = 1 && icy then (
        let socks = open_port ~icy (port + 1) :: socks in
        Hashtbl.replace opened_ports port (h, socks) )
      else ();
      h
    with Not_found ->
      (* First the port without icy *)
      let socks = [open_port ~icy:false port] in
      (* Now the port with icy, is requested.*)
      let socks = if icy then open_port ~icy (port + 1) :: socks else socks in
      let h = { sources = Hashtbl.create 1; http = Hashtbl.create 1 } in
      Hashtbl.add opened_ports port (h, socks);
      h

  (* Add sources... *)
  let add_source ~port ~mountpoint ~icy source =
    let sources =
      let handler = get_handler ~icy port in
      if Hashtbl.mem handler.sources mountpoint then raise Registered else ();
      handler.sources
    in
    log#important "Adding mountpoint '%s' on port %i" mountpoint port;
    Hashtbl.add sources mountpoint source

  (* Remove source. *)
  let remove_source ~port ~mountpoint () =
    let handler, socks = Hashtbl.find opened_ports port in
    assert (Hashtbl.mem handler.sources mountpoint);
    log#important "Removing mountpoint '%s' on port %i" mountpoint port;
    Hashtbl.remove handler.sources mountpoint;
    if Hashtbl.length handler.sources = 0 && Hashtbl.length handler.http = 0
    then (
      log#important "Nothing more on port %i: closing sockets." port;
      let f in_s =
        ignore (Unix.write in_s (Bytes.of_string " ") 0 1);
        Unix.close in_s
      in
      List.iter f socks;
      Hashtbl.remove opened_ports port )
    else ()

  (* Add http_handler... *)
  let add_http_handler ~port ~verb ~uri h =
    let handler = get_handler ~icy:false port in
    log#important "Adding handler for '%s %s' on port %i" (string_of_verb verb)
      uri port;
    if Hashtbl.mem handler.http (verb, uri) then
      log#important "WARNING: Handler already registered, old one removed!"
    else ();
    Hashtbl.replace handler.http (verb, uri) h

  (* Remove http_handler. *)
  let remove_http_handler ~port ~verb ~uri () =
    let handler, socks = Hashtbl.find opened_ports port in
    assert (Hashtbl.mem handler.http (verb, uri));
    log#important "Removing handler for '%s %s' on port %i"
      (string_of_verb verb) uri port;
    Hashtbl.remove handler.http (verb, uri);
    if Hashtbl.length handler.sources = 0 && Hashtbl.length handler.http = 0
    then (
      log#info "Nothing more on port %i: closing sockets." port;
      let f in_s =
        ignore (Unix.write in_s (Bytes.of_string " ") 0 1);
        Unix.close in_s
      in
      List.iter f socks;
      Hashtbl.remove opened_ports port )
    else ()
end

module Unix = Make (Unix_transport)
include Unix
