(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let () =
  Ssl_threads.init ();
  Ssl.init ()

let log = Log.make ["ssl"]

module Http = Liq_http

let protocol_of_value protocol_val =
  match Lang.to_string protocol_val with
    | "ssl.3" -> Ssl.SSLv3 [@alert "-deprecated"]
    | "tls.1" -> Ssl.TLSv1 [@alert "-deprecated"]
    | "tls.1.1" -> Ssl.TLSv1_1 [@alert "-deprecated"]
    | "tls.1.2" -> Ssl.TLSv1_2
    | "tls.1.3" -> Ssl.TLSv1_3
    | _ ->
        raise (Error.Invalid_value (protocol_val, "Invalid SSL protocol", []))

(* Translate SSL would-block conditions into the Unix errors raised by plain
   sockets, so callers handling non-blocking I/O (e.g. the output.harbor write
   loop) work identically on both transports. *)
let read_wrapper ssl buf ofs len =
  try Ssl.read ssl buf ofs len with
    | Ssl.Read_error (Ssl.Error_want_read | Ssl.Error_want_write) ->
        raise (Unix.Unix_error (Unix.EAGAIN, "read", ""))
    | Ssl.Read_error Ssl.Error_zero_return -> 0

let write_wrapper ssl buf ofs len =
  try Ssl.write ssl buf ofs len
  with Ssl.Write_error (Ssl.Error_want_read | Ssl.Error_want_write) ->
    raise (Unix.Unix_error (Unix.EAGAIN, "write", ""))

let ssl_socket ~pos transport ssl =
  let closed = Atomic.make false in
  let finalise s =
    if not (Atomic.get closed) then (
      let pos =
        match pos with
          | [] -> "unknown"
          | _ ->
              String.concat ", " (List.map (fun pos -> Pos.to_string pos) pos)
      in
      log#critical
        "SSL socket closed during garbage collection, you must have a leak in \
         your application! Socket opened at position: %s"
        pos;
      try s#close with _ -> ())
  in
  let s =
    object
      method typ = "ssl"
      method transport = transport
      method file_descr = Ssl.file_descr_of_socket ssl

      method wait_for ?log event timeout =
        let event =
          match event with
            | `Read -> `Read (Ssl.file_descr_of_socket ssl)
            | `Write -> `Write (Ssl.file_descr_of_socket ssl)
            | `Both -> `Both (Ssl.file_descr_of_socket ssl)
        in
        Tutils.wait_for ?log event timeout

      method read = read_wrapper ssl
      method write = write_wrapper ssl
      method closed = Atomic.get closed

      method close =
        Atomic.set closed true;
        let fd = Ssl.file_descr_of_socket ssl in
        Fun.protect
          ~finally:(fun () -> Unix.close fd)
          (fun () -> ignore (Ssl.close_notify ssl))
    end
  in
  Gc.finalise finalise s;
  s

let server_context ~min_protocol ~max_protocol ~password ~certificate ~key () =
  try
    let context =
      Ssl.create_context (Ssl.SSLv23 [@alert "-deprecated"]) Ssl.Server_context
    in
    Option.iter (Ssl.set_min_protocol_version context) min_protocol;
    Option.iter (Ssl.set_max_protocol_version context) max_protocol;
    Option.iter
      (fun password -> Ssl.set_password_callback context (fun _ -> password))
      password;
    let cert_path = certificate () in
    let key_path = Option.value ~default:cert_path (key ()) in
    Ssl.use_certificate context cert_path key_path;
    context
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Lang.raise_as_runtime ~bt ~kind:"ssl" exn

let server ~read_timeout ~write_timeout ~context transport =
  object
    method transport = transport

    method accept ?timeout sock =
      let s, caller = Http.accept ?timeout sock in
      try
        (match timeout with
          | Some timeout ->
              Http.set_socket_default ~read_timeout:timeout
                ~write_timeout:timeout s
          | None -> ());
        let ssl_s = Ssl.embed_socket s (context ()) in
        Ssl.accept ssl_s;
        Http.set_socket_default ~read_timeout ~write_timeout s;
        (ssl_socket ~pos:[] transport ssl_s, caller)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Unix.close s;
        Printexc.raise_with_backtrace exn bt
  end

(* OpenSSL reports success loading its default verify paths even when no CA
   store exists there, so an empty store only surfaces as error 20. *)
let verify_error_hint = function
  | 2 (* X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT *) ->
      ". A trusted certificate's issuer is missing: the transport's \
       certificate argument should be a root certificate, not an intermediate \
       one such as a Let's Encrypt chain.pem."
  | 9 (* X509_V_ERR_CERT_NOT_YET_VALID *) ->
      ". Check that the system clock is correct."
  | 10 (* X509_V_ERR_CERT_HAS_EXPIRED *) ->
      ". The server's certificate must be renewed, unless the system clock is \
       wrong."
  | 18 (* X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT *) ->
      ". To trust a self-signed server certificate, pass it with the \
       transport's certificate argument."
  | 19 (* X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN *) ->
      ". The server's root certificate is not trusted: pass it with the \
       transport's certificate argument."
  | 20 (* X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY *) ->
      ". No trusted root was found: check that a CA store is installed, set \
       SSL_CERT_FILE to point to one or pass the server's root certificate \
       with the transport's certificate argument. This also happens when the \
       server does not send its intermediate certificates, e.g. when using a \
       Let's Encrypt cert.pem instead of fullchain.pem."
  | 21 (* X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE *) ->
      ". The server should send its full certificate chain, e.g. a Let's \
       Encrypt fullchain.pem instead of cert.pem."
  | 22 (* X509_V_ERR_CERT_CHAIN_TOO_LONG *) ->
      ". The server's certificate chain exceeds the maximum verification depth \
       of 3."
  | _ -> ""

let transport ~min_protocol ~max_protocol ~read_timeout ~write_timeout
    ~server_context ~certificate () =
  object (self)
    method name = "ssl"
    method protocol = "https"
    method default_port = 443

    method connect ?bind_address ?timeout ?prefer host port =
      try
        let ctx =
          Ssl.create_context (Ssl.SSLv23 [@alert "-deprecated"])
            Ssl.Client_context
        in
        Option.iter (Ssl.set_min_protocol_version ctx) min_protocol;
        Option.iter (Ssl.set_max_protocol_version ctx) max_protocol;
        (* TODO: add option.. *)
        Ssl.set_verify ctx [] (Some Ssl.client_verify_callback);
        (* Add certificate from transport if passed. *)
        (try
           let cert = Utils.read_all (certificate ()) in
           Ssl.add_cert_to_store ctx cert
         with _ -> ());
        Ssl.set_verify_depth ctx 3;
        ignore (Ssl.set_default_verify_paths ctx);
        let unix_socket =
          Http.connect ?bind_address ?timeout ?prefer host port
        in
        try
          let socket = Ssl.embed_socket unix_socket ctx in
          (try Ssl.set_client_SNI_hostname socket host with _ -> ());
          Ssl.connect socket;
          let err = Ssl.get_verify_result socket in
          if err <> 0 then
            Runtime_error.raise ~pos:[]
              ~message:
                (Printf.sprintf "SSL verification error: %s%s"
                   (Ssl.get_verify_error_string err)
                   (verify_error_hint err))
              "ssl";
          ssl_socket ~pos:[] self socket
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Unix.close unix_socket;
          Printexc.raise_with_backtrace exn bt
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"ssl" exn

    method server =
      ignore (server_context ());
      server ~read_timeout ~write_timeout ~context:server_context self
  end

let _ =
  Lang.add_builtin ~base:Modules.http_transport "ssl" ~category:`Internet
    ~descr:"Https transport using libssl"
    [
      ( "read_timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some "Read timeout" );
      ( "write_timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some "Write timeout" );
      ( "password",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "SSL certificate password" );
      ( "min_protocol",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Minimal accepted SSL protocol. One of, from least recent to most \
           recent: `\"ssl.3\"`, `\"tls.1\"`, `\"tls.1.1\"`, `\"tls.1.2\"` or \
           `\"tls.1.3\"`. The most recent available protocol between client \
           and server is negotiated when initiating communication between \
           minimal and maximal protocol version. All protocols up to \
           `\"tls.1.2\"` and above are now deprecated so you might want to set \
           this value to one of those two. Default to lowest support protocol \
           if not set." );
      ( "max_protocol",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Maximal accepted SSL protocol. One of, from least recent to most \
           recent: `\"ssl.3\"`, `\"tls.1\"`, `\"tls.1.1\"`, `\"tls.1.2\"` or \
           `\"tls.1.3\"`. The most recent available protocol between client \
           and server is negotiated when initiating communication between \
           minimal and maximal protocol version. Defaults to highest protocol \
           supported if not set." );
      Lang.reload_on_arg;
      ( "certificate",
        Lang.getter_t (Lang.nullable_t Lang.string_t),
        Some Lang.null,
        Some
          "Path to certificate file. Required in server mode, e.g. \
           `input.harbor`, etc. If passed in client mode, certificate is added \
           to the list of valid certificates. In server mode, read when the \
           first port opens and on `reload()`." );
      ( "key",
        Lang.getter_t (Lang.nullable_t Lang.string_t),
        Some Lang.null,
        Some
          "Path to certificate private key. Required in server mode, e.g. \
           `input.harbor`, etc., unless the certificate file also contains the \
           private key. Read when the first port opens and on `reload()`." );
    ]
    Lang.reloadable_http_transport_t
    (fun p ->
      let read_timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "read_timeout" p)
      in
      let read_timeout =
        Option.value ~default:Harbor.conf_timeout#get read_timeout
      in
      let write_timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "write_timeout" p)
      in
      let write_timeout =
        Option.value ~default:Harbor.conf_timeout#get write_timeout
      in
      let password =
        Lang.to_valued_option Lang.to_string (List.assoc "password" p)
      in
      let min_protocol =
        Option.map protocol_of_value
          (Lang.to_option (List.assoc "min_protocol" p))
      in
      let max_protocol =
        Option.map protocol_of_value
          (Lang.to_option (List.assoc "max_protocol" p))
      in
      let path name =
        Lang.to_valued_option Lang.to_string
          (Lang.to_getter (List.assoc name p) ())
      in
      let certificate () =
        match path "certificate" with
          | None ->
              Runtime_error.raise ~pos:(Lang.pos p)
                "Cannot find certificate file!"
          | Some path -> Utils.check_readable ~pos:(Lang.pos p) path
      in
      let key () =
        match path "key" with
          | None -> None
          | Some path -> Some (Utils.check_readable ~pos:(Lang.pos p) path)
      in
      let server_context, reload =
        Lang.reloadable_server_config ~reload_on:(List.assoc "reload_on" p)
          (server_context ~min_protocol ~max_protocol ~password ~certificate
             ~key)
      in
      let transport =
        transport ~min_protocol ~max_protocol ~read_timeout ~write_timeout
          ~server_context ~certificate ()
      in
      Lang.reloadable_http_transport ~reload transport)
