(*****************************************************************************

    Liquidsoap, a programmable stream generator.
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
module Type = Liquidsoap_lang.Type
module Http = Liq_http

let ( let* ) = Duppy.Monad.bind

module type Monad_t = module type of Monad with module Io := Monad.Io

module type Transport_t = sig
  type socket = Http.socket

  val file_descr_of_socket : socket -> Unix.file_descr
  val read : socket -> bytes -> int -> int -> int
  val write : socket -> bytes -> int -> int -> int
  val close : socket -> unit

  module Duppy : sig
    module Io : Duppy.Io_t with type socket = socket

    module Monad : sig
      module Io :
        Duppy.Monad.Monad_io_t with type socket = socket and module Io = Io

      include Monad_t
    end
  end

  module Websocket : Websocket.Websocket_t with type socket = socket
end

module Http_transport = struct
  type socket = Http.socket

  let file_descr_of_socket socket = socket#file_descr
  let read socket = socket#read
  let write socket = socket#write
  let close socket = socket#close

  module Duppy_transport : Duppy.Transport_t with type t = Http.socket = struct
    type t = Http.socket

    type bigarray =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    let sock socket = socket#file_descr
    let read = read
    let write = write
    let ba_write _ _ _ _ = failwith "Not implemented!"
  end

  module Duppy = struct
    module Io = Duppy.MakeIo (Duppy_transport)

    module Monad = struct
      module Io = Duppy.Monad.MakeIo (Io)
      include (Monad : Monad_t)
    end
  end

  module Websocket_transport = struct
    type socket = Http.socket

    let read = read
    let read_retry socket = Extralib.read_retry socket#read
    let write = write
  end

  module Websocket = Websocket.Make (Websocket_transport)
end

module type T = sig
  type socket

  exception Retry
  exception Assoc of string
  exception Not_authenticated
  exception Unknown_codec
  exception Mount_taken
  exception Websocket_closed
  exception Protocol_not_supported of string

  (* Generic *)

  val file_descr_of_socket : socket -> Unix.file_descr
  val read : socket -> bytes -> int -> int -> int
  val write : socket -> bytes -> int -> int -> int
  val close : socket -> unit

  (* Http Server *)

  type http_verb = [ `Get | `Post | `Put | `Delete | `Head | `Options ]

  type reply =
    | Close of (unit -> string)
    | Relay of string * (unit -> unit)
    | Custom

  type http_handler =
    protocol:string ->
    meth:http_verb ->
    data:(float -> string) ->
    headers:(string * string) list ->
    query:(string * string) list ->
    socket:socket ->
    string ->
    (reply, reply) Duppy.Monad.t

  val verb_of_string : string -> http_verb
  val string_of_verb : http_verb -> string
  val mk_simple : string -> unit -> string
  val simple_reply : string -> ('a, reply) Duppy.Monad.t
  val reply : (unit -> string) -> ('a, reply) Duppy.Monad.t
  val custom : unit -> ('a, reply) Duppy.Monad.t

  val add_http_handler :
    pos:Liquidsoap_lang.Pos.t list ->
    transport:Http.transport ->
    port:int ->
    verb:http_verb ->
    uri:Lang.regexp ->
    http_handler ->
    unit

  val remove_http_handler :
    port:int -> verb:http_verb -> uri:Lang.regexp -> unit -> unit

  (* Source input *)

  class virtual source : object
    inherit Source.source

    method virtual relay :
      string ->
      (string * string) list ->
      ?read:(socket -> bytes -> int -> int -> int) ->
      socket ->
      unit

    method virtual encode_metadata : Frame.metadata -> unit
    method virtual login : string * (socket:socket -> string -> string -> bool)
    method virtual icy_charset : string option
    method virtual meta_charset : string option
    method virtual get_mime_type : string option
  end

  val http_auth_check :
    ?query:(string * string) list ->
    login:string * (socket:socket -> string -> string -> bool) ->
    socket ->
    (string * string) list ->
    (unit, reply) Duppy.Monad.t

  val relayed : string -> (unit -> unit) -> ('a, reply) Duppy.Monad.t

  val add_source :
    pos:Liquidsoap_lang.Pos.t list ->
    transport:Http.transport ->
    port:int ->
    mountpoint:string ->
    icy:bool ->
    source ->
    unit

  val remove_source : port:int -> mountpoint:string -> unit -> unit
end

module Make (T : Transport_t) : T with type socket = T.socket = struct
  module Websocket = T.Websocket
  module Task = Duppy.Task
  module Duppy = T.Duppy

  type socket = T.socket

  let file_descr_of_socket = T.file_descr_of_socket
  let read = T.read
  let write = T.write
  let close = T.close

  let protocol_name h =
    Printf.sprintf "%s/%s" h.Duppy.Monad.Io.socket#transport#protocol
      h.Duppy.Monad.Io.socket#transport#name

  (* Define what we need as a source *)

  (** Raised when source needs to retry read. *)
  exception Retry

  class virtual source =
    object
      inherit Source.source ~name:"input.harbor" ()

      method virtual relay
          : string ->
            (string * string) list ->
            ?read:(socket -> bytes -> int -> int -> int) ->
            socket ->
            unit

      method virtual encode_metadata : Frame.metadata -> unit

      method virtual login
          : string * (socket:socket -> string -> string -> bool)

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

  let string_of_any_verb = function
    | `Source -> "SOURCE"
    | `Shout -> "ICY"
    | `Get -> "GET"
    | `Post -> "POST"
    | `Put -> "PUT"
    | `Delete -> "DELETE"
    | `Head -> "HEAD"
    | `Options -> "OPTIONS"

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

  type reply =
    | Close of (unit -> string)
    | Relay of string * (unit -> unit)
    | Custom

  let mk_simple s =
    let ret = Atomic.make s in
    fun () -> Atomic.exchange ret ""

  let simple_reply s = Duppy.Monad.raise (Close (mk_simple s))
  let reply s = Duppy.Monad.raise (Close s)
  let relayed s f = Duppy.Monad.raise (Relay (s, f))
  let custom () = Duppy.Monad.raise Custom

  type http_handler =
    protocol:string ->
    meth:http_verb ->
    data:(float -> string) ->
    headers:(string * string) list ->
    query:(string * string) list ->
    socket:socket ->
    string ->
    (reply, reply) Duppy.Monad.t

  type http_handlers = (http_verb * Lang.regexp * http_handler) list Atomic.t
  type handler = { sources : sources; http : http_handlers }

  type open_port = {
    handler : handler;
    transport : Http.transport;
    fds : Unix.file_descr list;
  }

  let opened_ports : (int, open_port) Hashtbl.t = Hashtbl.create 1
  let find_handler = Hashtbl.find opened_ports

  let find_source mount port =
    Hashtbl.find (find_handler port).handler.sources mount

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
    let { Liq_http.user = requested_user; password } =
      try Liq_http.parse_auth r
      with Not_found -> { Liq_http.user = ""; password = r }
    in
    let* s =
      try Duppy.Monad.return (find_source "/" (port - 1))
      with Not_found ->
        log#info "ICY error: no / mountpoint";
        simple_reply "No / mountpoint\r\n\r\n"
    in
    (* Authentication can be blocking. *)
    Duppy.Monad.Io.exec ~priority:`Maybe_blocking h
      (let user, auth_f = s#login in
       let user = if requested_user = "" then user else requested_user in
       if auth_f ~socket:h.Duppy.Monad.Io.socket user password then
         Duppy.Monad.return (`Shout, "/", `Icy)
       else (
         log#info "ICY error: invalid password";
         simple_reply "Invalid password\r\n\r\n"))

  exception Protocol_not_supported of string

  let parse_http_request_line r =
    try
      let data = Re.Pcre.split ~rex:(Re.Pcre.regexp "[ \t]+") r in
      let protocol = verb_or_source_of_string (List.nth data 0) in
      Duppy.Monad.return
        ( protocol,
          List.nth data 1,
          match String.uppercase_ascii (List.nth data 2) with
            | "HTTP/1.0" -> `Http_10
            | "HTTP/1.1" -> `Http_11
            | "ICE/1.0" -> `Ice_10
            | s when protocol = `Source -> `Xaudiocast_uri s
            | s -> raise (Protocol_not_supported s) )
    with
      | Protocol_not_supported p ->
          log#info "Protocol not supported for request %s: %s" r p;
          simple_reply "HTTP 505 Protocol Not Supported\r\n\r\n"
      | e ->
          log#info "Invalid request line %s: %s" r (Printexc.to_string e);
          simple_reply "HTTP 500 Invalid request\r\n\r\n"

  let parse_headers headers =
    let split_header h l =
      try
        let rex = Re.Pcre.regexp "([^:\\r\\n]+):\\s*([^\\r\\n]+)" in
        let sub = Re.Pcre.exec ~rex h in
        (Re.Pcre.get_substring sub 1, Re.Pcre.get_substring sub 2) :: l
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

  let auth_check ~auth_f socket user pass =
    (* OK *)
    if conf_pass_verbose#get then
      log#info "Requested username: %s, password: %s." user pass
    else ();
    if not (auth_f ~socket user pass) then raise Not_authenticated else ();
    log#info "Client logged in.";
    Duppy.Monad.return ()

  let http_auth_check ?query ~login socket headers =
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
          let data = Re.Pcre.split ~rex:(Re.Pcre.regexp "[ \t]+") auth in
          match data with
            | "Basic" :: x :: _ ->
                let { Liq_http.user; password } =
                  Liq_http.parse_auth (Lang_string.decode64 x)
                in
                (user, password)
            | _ -> raise Not_found
        with Not_found -> (
          match query with
            | Some query ->
                (* ICY updates are done with
                         * password sent in GET args
                         * and user being valid_user
                         * or user, if given. *)
                let user =
                  try List.assoc "user" query with Not_found -> valid_user
                in
                (user, List.assoc "pass" query)
            | _ -> raise Not_found)
      in
      auth_check ~auth_f socket user pass
    with
      | Not_authenticated ->
          log#info "Returned 401: wrong auth.";
          http_reply "Wrong Authentication data"
      | Not_found ->
          log#info "Returned 401: bad authentication.";
          http_reply "No login / password supplied."

  let exec_http_auth_check ?args ~login h headers =
    let query =
      Option.map
        (fun query ->
          Hashtbl.fold (fun lbl k query -> (lbl, k) :: query) query [])
        args
    in
    Duppy.Monad.Io.exec ~priority:`Maybe_blocking h
      (http_auth_check ?query ~login h.Duppy.Monad.Io.socket headers)

  (* We do not implement anything with this handler for now. *)
  let handle_asterisk_options_request ~hprotocol:_ ~headers:_ _ =
    log#info "Returned 405: Method Not Allowed";
    simple_reply
      "HTTP/1.0 405 Method Not Allowed\r\n\
       Allow: OPTIONS, GET, POST, PUT\r\n\
       \r\n"

  let handle_source_request ~port ~auth ~smethod hprotocol h uri headers =
    (* ICY request are on port+1 *)
    let source_port = if smethod = `Shout then port - 1 else port in
    let* s =
      try Duppy.Monad.return (find_source uri source_port)
      with Not_found ->
        log#info "Request failed: no mountpoint '%s'!" uri;
        simple_reply
          (http_error_page 404 "Not found" "This mountpoint isn't available.")
    in
    let* () =
      if
        (* ICY and Xaudiocast auth check was done before.. *)
        not auth
      then exec_http_auth_check ~login:s#login h headers
      else Duppy.Monad.return ()
    in
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
              Buffer.add_substring buf s 0 len);
            let len = min len (Buffer.length buf) in
            Buffer.blit buf 0 b ofs len;
            Utils.buffer_drop buf len;
            len
          in
          Some read)
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
            (http_error_page 404 "Not found" "This mountpoint isn't available.")
      | Unknown_codec ->
          log#info "Returned 501: unknown audio codec";
          simple_reply
            (http_error_page 501 "Not Implemented"
               "This stream's format is not recognized.")
      | e ->
          log#info "Returned 500 for '%s': %s" uri (Printexc.to_string e);
          simple_reply
            (http_error_page 500 "Internal Server Error"
               "The server could not handle your request.")

  exception Websocket_closed

  let handle_websocket_request ~port h mount headers =
    let json_string_of = function `String s -> s | _ -> raise Not_found in
    let extract_packet s =
      let json =
        match Json.from_string s with
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
    let websocket_read = Websocket.read () in
    let read_hello s =
      let error () = simple_reply (websocket_error 1002 "Invalid hello.") in
      try
        match websocket_read s with
          | `Text s -> (
              log#debug "Hello packet: %s\n%!" s;
              match extract_packet s with
                | "hello", data ->
                    let data = Option.get data in
                    let mime = json_string_of (List.assoc "mime" data) in
                    let user = json_string_of (List.assoc "user" data) in
                    let password =
                      json_string_of (List.assoc "password" data)
                    in
                    Duppy.Monad.return (mime, mount, user, password)
                | _ -> error ())
          | _ -> error ()
      with _ -> error ()
    in
    let* () =
      Duppy.Monad.Io.write ?timeout:(Some conf_timeout#get)
        ~priority:`Non_blocking h
        (Bytes.of_string (Websocket.upgrade headers))
    in
    let* stype, huri, user, password =
      Duppy.Monad.Io.exec ~priority:`Blocking h
        (read_hello h.Duppy.Monad.Io.socket)
    in
    log#info "Mime type: %s" stype;
    log#info "Mount point: %s" huri;
    let* source =
      try Duppy.Monad.return (find_source huri port)
      with Not_found ->
        log#info "Request failed: no mountpoint '%s'!" huri;
        simple_reply (websocket_error 1011 "This mountpoint isn't available.")
    in
    let _, auth_f = source#login in
    let* () =
      try auth_check ~auth_f h.Duppy.Monad.Io.socket user password
      with Not_authenticated ->
        log#info "Authentication failed!";
        simple_reply (websocket_error 1011 "Authentication failed.")
    in
    let binary_data = Buffer.create Utils.pagesize in
    let read_socket socket =
      match websocket_read socket with
        | `Binary buf -> Buffer.add_string binary_data buf
        | `Text s -> (
            match extract_packet s with
              | "metadata", data ->
                  log#debug "Metadata packet: %s\n%!" s;
                  let data = Option.get data in
                  let m = List.map (fun (l, v) -> (l, json_string_of v)) data in
                  let m = Frame.Metadata.from_list m in
                  source#encode_metadata m;
                  raise Retry
              | _ -> raise Retry)
        | `Close _ -> raise Websocket_closed
        | _ -> raise Retry
    in
    let read socket buf ofs len =
      if Buffer.length binary_data = 0 then read_socket socket else ();
      let len = min (Buffer.length binary_data) len in
      Buffer.blit binary_data 0 buf ofs len;
      Utils.buffer_drop binary_data len;
      len
    in
    let f () = source#relay stype headers ~read h.Duppy.Monad.Io.socket in
    relayed "" f

  exception Handled of (http_verb * (string * string) list * http_handler)

  let ans_404 uri =
    log#info "Returned 404 for '%s'." uri;
    simple_reply (http_error_page 404 "Not found" "This page isn't available.")

  let ans_500 uri =
    log#info "Returned 500 for '%s'." uri;
    simple_reply
      (http_error_page 500 "Internal Server Error"
         "There was an error processing your request.")

  let ans_400 ~uri s =
    log#info "Returned 400 for '%s': %s." uri s;
    simple_reply (http_error_page 400 "Bad Request" s)

  let admin ~icy ~port ~uri ~headers ~args h =
    let* mode =
      try Duppy.Monad.return (Hashtbl.find args "mode")
      with Not_found -> ans_400 ~uri "unrecognised command"
    in
    let len =
      try int_of_string (assoc_uppercase "CONTENT-LENGTH" headers) with _ -> 0
    in
    let* data =
      if len > 0 then
        Duppy.Monad.Io.read ?timeout:(Some conf_timeout#get)
          ~priority:`Non_blocking ~marker:(Duppy.Io.Length len) h
      else Duppy.Monad.return ""
    in
    (try
       if
         assoc_uppercase "CONTENT-TYPE" headers
         = "application/x-www-form-urlencoded"
       then Hashtbl.iter (Hashtbl.replace args) (Http.args_split data)
     with Not_found -> ());
    match mode with
      | "updinfo" ->
          let mount = try Hashtbl.find args "mount" with Not_found -> "/" in
          log#info "Request to update metadata for mount %s on port %i" mount
            port;
          let* s =
            try Duppy.Monad.return (find_source mount port)
            with Not_found -> ans_400 ~uri "Source is not available"
          in
          let* () = exec_http_auth_check ~args ~login:s#login h headers in
          let* () =
            if
              not
                (List.mem
                   (Option.value ~default:"" s#get_mime_type)
                   conf_icy_metadata#get)
            then (
              log#info
                "Returned 405 for '%s': Source format does not support ICY \
                 metadata update"
                uri;
              simple_reply
                (http_error_page 405 "Method Not Allowed" "Method Not Allowed"))
            else Duppy.Monad.return ()
          in
          Hashtbl.remove args "mount";
          Hashtbl.remove args "mode";
          let in_enc =
            try
              let enc = Charset.of_string (Hashtbl.find args "charset") in
              Hashtbl.remove args "charset";
              Some enc
            with Not_found ->
              (if icy then s#icy_charset else s#meta_charset)
              |> Option.map Charset.of_string
          in
          (* Recode tags.. *)
          let g x = Charset.convert ?source:in_enc x in
          let f x y m =
            let add, x =
              match x with
                | "song" when not conf_map_song_metadata#get -> (true, "song")
                | "song" ->
                    ( not (Hashtbl.mem args "title" || Hashtbl.mem args "artist"),
                      "title" )
                | "url" -> (true, "metadata_url")
                | _ -> (true, x)
            in
            if add then Frame.Metadata.add (g x) (g y) m else m
          in
          let args = Hashtbl.fold f args Frame.Metadata.empty in
          s#encode_metadata args;
          simple_reply
            (Printf.sprintf
               "HTTP/1.0 200 OK\r\n\r\nUpdated metadatas for mount %s" mount)
      | _ -> ans_500 uri

  let handle_http_request ~hmethod ~hprotocol ~port h uri headers =
    let rex = Re.Pcre.regexp "^(.+)\\?(.+)$" in
    let base_uri, args =
      try
        let sub = Re.Pcre.exec ~rex uri in
        (Re.Pcre.get_substring sub 1, Re.Pcre.get_substring sub 2)
      with Not_found -> (uri, "")
    in
    let smethod = string_of_verb hmethod in
    log#info "%s %s request on %s." (protocol_name h) smethod base_uri;
    let args = Http.args_split args in
    (* Filter out password *)
    let log_args =
      if conf_pass_verbose#get then args
      else (
        let log_args = Hashtbl.copy args in
        Hashtbl.remove log_args "pass";
        log_args)
    in
    Hashtbl.iter (log#info "%s Arg: %s, value: %s." (protocol_name h)) log_args;

    (* First, try with a registered handler. *)
    let { handler; _ } = find_handler port in
    let f (verb, regex, handler) =
      let rex = regex.Liquidsoap_lang.Builtins_regexp.regexp in
      let sub =
        Lazy.from_fun (fun () ->
            try Some (Re.Pcre.exec ~rex base_uri) with _ -> None)
      in
      if (verb :> verb) = hmethod && Lazy.force sub <> None then (
        let sub = Option.get (Lazy.force sub) in
        let groups =
          List.fold_left
            (fun groups name ->
              try (name, Re.Pcre.get_named_substring rex name sub) :: groups
              with Not_found -> groups)
            []
            (Array.to_list (Re.Pcre.names rex))
        in
        log#info "Found handler '%s %s' on port %d%s." smethod
          (Lang.descr_of_regexp regex)
          port
          (match groups with
            | [] -> ""
            | groups ->
                Printf.sprintf " with matches: %s"
                  (String.concat ", "
                     (List.map
                        (fun (lbl, v) -> [%string "%{lbl}: %{v}"])
                        groups)));
        raise (Handled (verb, groups, handler)))
      else ()
    in
    try
      List.iter f (Atomic.get handler.http);

      (* Otherwise, try with a standard handler. *)
      let is_admin s = try String.sub s 0 6 = "/admin" with _ -> false in
      match base_uri with
        (* Icecast *)
        | "/admin/metadata" -> admin ~icy:false ~port ~uri ~headers ~args h
        (* Shoutcast *)
        | "/admin.cgi" -> admin ~icy:true ~port ~uri ~headers ~args h
        | s when is_admin s -> ans_400 ~uri "unrecognised command"
        | _ -> ans_404 uri
    with
      | Handled (meth, groups, handler) ->
          let protocol =
            match hprotocol with
              | `Http_10 -> "1.0"
              | `Http_11 -> "1.1"
              | _ -> assert false
          in
          let query =
            groups @ Hashtbl.fold (fun lbl k query -> (lbl, k) :: query) args []
          in
          let headers =
            List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers
          in
          let content_type =
            try Some (assoc_uppercase "CONTENT-TYPE" headers) with _ -> None
          in
          let content_length =
            try Some (int_of_string (assoc_uppercase "CONTENT-LENGTH" headers))
            with _ -> None
          in
          let transfer_encoding =
            try Some (assoc_uppercase "TRANSFER-ENCODING" headers)
            with _ -> None
          in
          let socket : Http.socket =
            let rem_data = h.Duppy.Monad.Io.data in
            let rem_len = String.length rem_data in
            let rem_ofs = Atomic.make 0 in
            let socket = h.Duppy.Monad.Io.socket in
            object
              method typ = socket#typ
              method transport = socket#transport
              method file_descr = socket#file_descr
              method write = socket#write
              method close = socket#close
              method closed = socket#closed

              method wait_for ?log event timeout =
                match (event, Atomic.get rem_ofs) with
                  | `Read, ofs when ofs < rem_len -> ()
                  | _ -> socket#wait_for ?log event timeout

              method read buf dst_ofs len =
                if Atomic.get rem_ofs < rem_len then (
                  let src_ofs = Atomic.get rem_ofs in
                  let len = min (rem_len - src_ofs) len in
                  Bytes.blit_string rem_data src_ofs buf dst_ofs len;
                  Atomic.set rem_ofs (src_ofs + len);
                  len)
                else socket#read buf dst_ofs len
            end
          in

          let data =
            match (content_type, content_length, transfer_encoding) with
              | _, Some len, _ when len > 0 ->
                  let buflen = min len 4096 in
                  let len = Atomic.make len in
                  fun timeout ->
                    let ret =
                      Http.read ~timeout socket (min (Atomic.get len) buflen)
                    in
                    Atomic.set len (Atomic.get len - String.length ret);
                    ret
              | _, _, Some "chunked" ->
                  fun timeout -> fst (Http.read_chunked ~timeout socket)
              | _ -> fun _ -> ""
          in
          Duppy.Monad.Io.exec ~priority:`Maybe_blocking h
            (handler ~protocol ~meth ~headers ~data
               ~socket:h.Duppy.Monad.Io.socket ~query base_uri)
      | e ->
          let bt = Printexc.get_backtrace () in
          Utils.log_exception ~log ~bt
            (Printf.sprintf "%s %s request on uri '%s' failed: %s"
               (protocol_name h) smethod (Printexc.to_string e) uri);
          ans_500 uri

  let handle_client ~port ~icy h =
    (* Read and process lines *)
    let* s =
      Duppy.Monad.Io.read ?timeout:(Some conf_timeout#get)
        ~priority:`Non_blocking
        ~marker:
          (match icy with
            | true -> Duppy.Io.Split "[\r]?\n"
            | false -> Duppy.Io.Split "[\r]?\n[\r]?\n")
        h
    in
    let lines = Re.Pcre.split ~rex:(Re.Pcre.regexp "[\r]?\n") s in
    let* hmethod, huri, hprotocol =
      let s = List.hd lines in
      if icy then parse_icy_request_line ~port h s
      else parse_http_request_line s
    in
    log#info "Method: %s, uri: %s, protocol: %s"
      (string_of_any_verb hmethod)
      huri
      (string_of_protocol hprotocol);
    let headers = parse_headers (List.tl lines) in
    let hprotocol =
      try
        if hmethod = `Get && assoc_uppercase "UPGRADE" headers <> "websocket"
        then raise Exit
        else ();
        if assoc_uppercase "SEC-WEBSOCKET-PROTOCOL" headers <> "webcast" then
          raise Exit
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
      let* auth, huri, smethod =
        (* X-audiocast sends lines of the form:
           [SOURCE password path] *)
          match hprotocol with
          | `Xaudiocast_uri uri ->
              let password = huri in
              (* We check authentication here *)
              let* s =
                try Duppy.Monad.return (find_source uri port)
                with Not_found ->
                  log#info "Request failed: no mountpoint '%s'!" uri;
                  simple_reply
                    (http_error_page 404 "Not found"
                       "This mountpoint isn't available.")
              in
              (* Authentication can be blocking *)
              Duppy.Monad.Io.exec
                ~priority:
                  (* ICY = true means that authentication has already
                     happened *)
                  `Maybe_blocking h
                (let valid_user, auth_f = s#login in
                 if
                   not
                     (auth_f ~socket:h.Duppy.Monad.Io.socket valid_user password)
                 then simple_reply "Invalid password!"
                 else Duppy.Monad.return (true, uri, `Xaudiocast))
          | _ -> Duppy.Monad.return (false, huri, smethod)
      in
      handle_source_request ~port ~auth ~smethod hprotocol h huri headers
    in
    match hmethod with
      | `Put when is_source -> handle_source `Put
      | `Post when is_source -> handle_source `Post
      | `Source when not icy -> handle_source `Source
      | `Get when hprotocol = `Websocket ->
          handle_websocket_request ~port h huri headers
      | `Options when huri = "*" ->
          handle_asterisk_options_request ~hprotocol ~headers h
      | (`Get | `Post | `Put | `Delete | `Options | `Head) when not icy ->
          handle_http_request ~hmethod ~hprotocol ~port h huri headers
      | `Shout when icy ->
          let* () =
            Duppy.Monad.Io.write ?timeout:(Some conf_timeout#get)
              ~priority:`Non_blocking h
              (Bytes.of_string "OK2\r\nicy-caps:11\r\n\r\n")
          in
          (* Now parsing headers *)
          let* s =
            Duppy.Monad.Io.read ?timeout:(Some conf_timeout#get)
              ~priority:`Non_blocking ~marker:(Duppy.Io.Split "[\r]?\n[\r]?\n")
              h
          in
          let lines = Re.Pcre.split ~rex:(Re.Pcre.regexp "[\r]?\n") s in
          let headers = parse_headers lines in
          handle_source_request ~port ~auth:true ~smethod:`Shout hprotocol h
            huri headers
      | _ ->
          log#info "Returned 501: not implemented";
          simple_reply
            (http_error_page 501 "Not Implemented"
               "The server did not understand your request.")

  (* {1 The server} *)
  (* Open a port and listen to it. *)
  let open_port ~transport ~icy port =
    log#info "Opening port %d with icy = %b" port icy;
    let max_conn = conf_harbor_max_conn#get in
    let server = transport#server in
    let process_client sock =
      try
        let socket, caller =
          server#accept ?timeout:(Some conf_accept_timeout#get) sock
        in
        let ip = Utils.name_of_sockaddr ~rev_dns:conf_revdns#get caller in
        log#info "New client on port %i: %s" port ip;
        let unix_socket = T.file_descr_of_socket socket in
        Unix.setsockopt unix_socket Unix.TCP_NODELAY true;
        let on_error e =
          (match e with
            | Duppy.Io.Io_error -> log#info "Client %s disconnected" ip
            | Duppy.Io.Timeout ->
                log#info "Timeout while communicating with client %s." ip
            | Duppy.Io.Unix (c, p, m, bt) ->
                Utils.log_exception ~log
                  ~bt:(Printexc.raw_backtrace_to_string bt)
                  (Printf.sprintf "Unix error: %s"
                     (Printexc.to_string (Unix.Unix_error (c, p, m))))
            | Duppy.Io.Unknown (exn, bt) ->
                Utils.log_exception ~log
                  ~bt:(Printexc.raw_backtrace_to_string bt)
                  (Printf.sprintf "Unknown error: %s" (Printexc.to_string exn)));

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
              | Custom -> ("", fun () -> ())
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
          Duppy.Io.write ~timeout:conf_timeout#get ~priority:`Non_blocking
            ~on_error ~string:(Bytes.of_string s) ~exec Tutils.scheduler socket
        in
        Duppy.Monad.run ~return:reply ~raise:reply (handle_client ~port ~icy h)
      with e ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log ~bt
          (Printf.sprintf "Failed to accept new client: %s"
             (Printexc.to_string e))
    in
    let rec incoming ~port ~icy events (out_s : Unix.file_descr) e =
      if List.mem (`Read out_s) e then (
        try
          List.iter
            (function `Read s -> Unix.close s | _ -> assert false)
            events;
          []
        with _ -> [])
      else (
        let get_sock = function `Read sock -> sock | _ -> assert false in
        List.iter process_client (List.map get_sock e);
        [
          {
            Task.priority = `Non_blocking;
            events;
            handler = incoming ~port ~icy events out_s;
          };
        ])
    in
    let open_socket port bind_addr =
      let bind_addr_inet = Unix.inet_addr_of_string bind_addr in
      let bind_addr = Unix.ADDR_INET (bind_addr_inet, port) in
      let domain = Unix.domain_of_sockaddr bind_addr in
      let sock = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
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
    let in_s, out_s = Unix.pipe ~cloexec:true () in
    let events = `Read in_s :: List.map (open_socket port) bind_addrs in
    Task.add Tutils.scheduler
      {
        Task.priority = `Non_blocking;
        events;
        handler = incoming ~port ~icy events in_s;
      };
    out_s

  (* This, contrary to the find_xx functions
     creates the handlers when they are missing. *)
  let get_handler ~pos ~transport ~icy port =
    try
      let { handler; fds; transport = t } = Hashtbl.find opened_ports port in
      if transport#name <> t#name then
        Lang.raise_error ~pos
          ~message:"Port is already opened with a different transport" "http";
      (* If we have only one socket and icy=true,
       * we need to open a second one. *)
      if List.length fds = 1 && icy then (
        let fds = open_port ~transport ~icy (port + 1) :: fds in
        Hashtbl.replace opened_ports port { handler; fds; transport })
      else ();
      handler
    with Not_found ->
      (* First the port without icy *)
      let fds = [open_port ~transport ~icy:false port] in
      (* Now the port with icy, is requested.*)
      let fds =
        if icy then open_port ~transport ~icy (port + 1) :: fds else fds
      in
      let handler = { sources = Hashtbl.create 1; http = Atomic.make [] } in
      Hashtbl.replace opened_ports port { handler; fds; transport };
      handler

  (* Add sources... This is tied up to sources lifecycle so
     no need to prevent early start *)
  let add_source ~pos ~transport ~port ~mountpoint ~icy source =
    let sources =
      let handler = get_handler ~pos ~transport ~icy port in
      if Hashtbl.mem handler.sources mountpoint then
        Lang.raise_error ~pos ~message:"Mountpoint is already taken!" "http"
      else ();
      handler.sources
    in
    log#important "Adding mountpoint '%s' on port %i" mountpoint port;
    Hashtbl.replace sources mountpoint source

  (* Remove source. *)
  let remove_source ~port ~mountpoint () =
    let { handler; fds; _ } = Hashtbl.find opened_ports port in
    assert (Hashtbl.mem handler.sources mountpoint);
    log#important "Removing mountpoint '%s' on port %i" mountpoint port;
    Hashtbl.remove handler.sources mountpoint;
    if
      Hashtbl.length handler.sources = 0
      && List.length (Atomic.get handler.http) = 0
    then (
      log#important "Nothing more on port %i: closing sockets." port;
      let f in_s =
        ignore (Unix.write in_s (Bytes.of_string " ") 0 1);
        Unix.close in_s
      in
      List.iter f fds;
      Hashtbl.remove opened_ports port)
    else ()

  (* Add http_handler... *)
  let add_http_handler ~pos ~transport ~port ~verb ~uri h =
    let exec () =
      let handler = get_handler ~pos ~transport ~icy:false port in
      let suri = Lang.descr_of_regexp uri in
      log#important "Adding handler for '%s %s' on port %i"
        (string_of_verb verb) suri port;
      Atomic.set handler.http (Atomic.get handler.http @ [(verb, uri, h)])
    in
    Server.on_start exec

  (* Remove http_handler. *)
  let remove_http_handler ~port ~verb ~uri () =
    let exec () =
      let { handler; fds; _ } = Hashtbl.find opened_ports port in
      let suri = Lang.descr_of_regexp uri in
      let handlers, removed =
        List.partition
          (fun (v, u, _) -> v = verb && suri = Lang.descr_of_regexp u)
          (Atomic.get handler.http)
      in
      if removed <> [] then
        log#important "Removing handler for '%s %s' on port %i"
          (string_of_verb verb) suri port;
      Atomic.set handler.http handlers;
      if
        Hashtbl.length handler.sources = 0
        && List.length (Atomic.get handler.http) = 0
      then (
        log#info "Nothing more on port %i: closing sockets." port;
        let f in_s =
          ignore (Unix.write in_s (Bytes.of_string " ") 0 1);
          Unix.close in_s
        in
        List.iter f fds;
        Hashtbl.remove opened_ports port)
      else ()
    in
    Server.on_start exec
end

module Harbor = Make (Http_transport)
include Harbor
