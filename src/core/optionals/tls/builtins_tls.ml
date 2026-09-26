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

let log = Log.make ["tls"]

module Http = Liq_http

module Liq_tls = struct
  type t = {
    read_pending : Strings.Mutable.t;
    fd : Unix.file_descr;
    buf : bytes;
    mutable state : Tls.Engine.state;
  }

  let () = Mirage_crypto_rng_unix.use_default ()
  let buf_len = 4096

  let string_of_alert_level = function
    | Tls.Packet.WARNING -> "Warning"
    | Tls.Packet.FATAL -> "Fatal"

  let string_of_failure error =
    let level, typ = Tls.Engine.alert_of_failure error in
    Printf.sprintf "%s error: %s"
      (string_of_alert_level level)
      (Tls.Packet.alert_type_to_string typ)

  let write_all ~timeout fd data =
    Tutils.write_all ~timeout fd (Bytes.unsafe_of_string data)

  let read ~timeout h len =
    Tutils.wait_for (`Read h.fd) timeout;
    let n = Unix_utils.read h.fd h.buf 0 (min len buf_len) in
    Bytes.sub_string h.buf 0 n

  let read_pending h = function
    | None -> ()
    | Some data -> Strings.Mutable.add h.read_pending (Cstruct.to_string data)

  let write_response ~timeout h = function
    | None -> ()
    | Some data -> write_all ~timeout h.fd data

  let handshake ~timeout h =
    let rec f () =
      if Tls.Engine.handshake_in_progress h.state then (
        match Tls.Engine.handle_tls h.state (read ~timeout h buf_len) with
          | Ok (_, Some `Eof, _, _) ->
              Runtime_error.raise ~pos:[]
                ~message:"Connection closed while negotiating TLS handshake!"
                "tls"
          | Ok (state, None, `Response response, `Data data) ->
              h.state <- state;
              read_pending h (Option.map Cstruct.of_string data);
              write_response ~timeout h response;
              f ()
          | Error (error, `Response response) ->
              write_all ~timeout h.fd response;
              Runtime_error.raise ~pos:[]
                ~message:
                  (Printf.sprintf "TLS handshake error: %s"
                     (string_of_failure error))
                "tls")
    in
    f ()

  let init_base ~timeout ~state fd =
    let buf = Bytes.create buf_len in
    let read_pending = Strings.Mutable.create () in
    let h = { read_pending; fd; buf; state } in
    handshake ~timeout h;
    h

  let init_server ~timeout ~server fd =
    let state = Tls.Engine.server server in
    init_base ~timeout ~state fd

  let init_client ~timeout ~client fd =
    let state, hello = Tls.Engine.client client in
    write_all ~timeout fd hello;
    init_base ~timeout ~state fd

  let write ?timeout h b off len =
    let timeout = Option.value ~default:Harbor.conf_timeout#get timeout in
    match
      Tls.Engine.send_application_data h.state [Bytes.sub_string b off len]
    with
      | None -> len
      | Some (state, data) ->
          write_all ~timeout h.fd data;
          h.state <- state;
          len

  let read ?read_timeout ?write_timeout h b off len =
    let read_timeout =
      Option.value ~default:Harbor.conf_timeout#get read_timeout
    in
    let write_timeout =
      Option.value ~default:Harbor.conf_timeout#get write_timeout
    in
    if not (Strings.Mutable.is_empty h.read_pending) then
      Strings.Mutable.take h.read_pending b off len
    else (
      let rec f () =
        match
          Tls.Engine.handle_tls h.state (read ~timeout:read_timeout h len)
        with
          | Ok (state, eof, `Response response, `Data data) -> (
              (match response with
                | None -> ()
                | Some r -> write_all ~timeout:write_timeout h.fd r);
              h.state <- state;
              match (eof, data) with
                | Some `Eof, None -> 0
                | _, None -> f ()
                | _, Some data ->
                    Strings.Mutable.add h.read_pending data;
                    Strings.Mutable.take h.read_pending b off len)
          | Error (error, `Response response) ->
              write_all ~timeout:write_timeout h.fd response;
              Runtime_error.raise ~pos:[]
                ~message:
                  (Printf.sprintf "TLS read error: %s" (string_of_failure error))
                "tls"
      in
      f ())

  let close ?(timeout = 1.) h =
    let state, data = Tls.Engine.send_close_notify h.state in
    write_all ~timeout h.fd data;
    h.state <- state;
    Unix.close h.fd
end

let tls_socket ~pos ~session transport =
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
    object (self)
      method typ = "tls"
      method transport = transport
      method file_descr = session.Liq_tls.fd

      method pending =
        not (Strings.Mutable.is_empty session.Liq_tls.read_pending)

      method wait_for ?log event timeout =
        Http.wait_for ?log ~pending:self#pending self#file_descr event timeout

      method read = Liq_tls.read session
      method write = Liq_tls.write session
      method closed = Atomic.get closed

      method close =
        Atomic.set closed true;
        Liq_tls.close session
    end
  in
  Gc.finalise finalise s;
  s

let server_config ~certificate ~key ~client_certificate () =
  try
    let certificate_path = certificate () in
    let key_path = Option.value ~default:certificate_path (key ()) in
    let certificate = Utils.read_all certificate_path in
    let certificates =
      Result.get_ok (X509.Certificate.decode_pem_multiple certificate)
    in
    let key =
      Result.get_ok (X509.Private_key.decode_pem (Utils.read_all key_path))
    in
    let authenticator =
      match client_certificate () with
        | None -> None
        | Some cert ->
            Some
              (X509.Authenticator.chain_of_trust
                 ~time:(fun () -> Some (Ptime_clock.now ()))
                 (Result.get_ok
                    (X509.Certificate.decode_pem_multiple (Utils.read_all cert))))
    in
    match
      Tls.Config.server
        ~certificates:(`Single (certificates, key))
        ?authenticator ()
    with
      | Ok server -> server
      | Error (`Msg message) -> Runtime_error.raise ~pos:[] ~message "tls"
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Lang.raise_as_runtime ~bt ~kind:"tls" exn

let server ~read_timeout ~write_timeout ~config transport =
  object
    method transport = transport

    method accept ?(timeout = 1.) sock =
      let fd, caller = Http.accept ~timeout sock in
      try
        Http.set_socket_default ~read_timeout:timeout ~write_timeout:timeout fd;
        let session = Liq_tls.init_server ~timeout ~server:(config ()) fd in
        Http.set_socket_default ~read_timeout ~write_timeout fd;
        (tls_socket ~pos:[] ~session transport, caller)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Unix.close fd;
        Printexc.raise_with_backtrace exn bt
  end

let transport ~read_timeout ~write_timeout ~server_config ~certificate
    ~client_certificate ~client_key () =
  object (self)
    method name = "tls"
    method protocol = "https"
    method default_port = 443

    method connect ?bind_address ?(timeout = 1.) ?prefer host port =
      let domain = Domain_name.host_exn (Domain_name.of_string_exn host) in
      let authenticator = Result.get_ok (Ca_certs.authenticator ()) in
      let certificate_authenticator =
        try
          let certificates =
            Result.get_ok
              (X509.Certificate.decode_pem_multiple
                 (Utils.read_all (certificate ())))
          in
          Some
            (X509.Authenticator.chain_of_trust
               ~time:(fun () -> Some (Ptime_clock.now ()))
               certificates)
        with _ -> None
      in
      let authenticator ?ip ~host certs =
        match certificate_authenticator with
          | None -> authenticator ?ip ~host certs
          | Some auth ->
              let r = auth ?ip ~host certs in
              if Result.is_ok r then r else authenticator ?ip ~host certs
      in
      let certificates =
        match client_certificate () with
          | None -> `None
          | Some cert ->
              `Single
                ( Result.get_ok
                    (X509.Certificate.decode_pem_multiple (Utils.read_all cert)),
                  Result.get_ok
                    (X509.Private_key.decode_pem
                       (Utils.read_all (client_key ()))) )
      in
      let client =
        match
          Tls.Config.client ~authenticator ~certificates ~peer_name:domain ()
        with
          | Ok client -> client
          | Error (`Msg message) -> Runtime_error.raise ~pos:[] ~message "tls"
      in
      let fd = Http.connect ?bind_address ~timeout ?prefer host port in
      let session = Liq_tls.init_client ~timeout ~client fd in
      tls_socket ~pos:[] ~session self

    method server =
      ignore (server_config ());
      server ~read_timeout ~write_timeout ~config:server_config self
  end

let () =
  Reloadable_transport.add_builtin ~base:Modules.http_transport
    ~transport_t:Lang.http_transport_t "tls"
    ~descr:"Https transport using libtls"
    [
      ( "read_timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some "Read timeout. Defaults to harbor's timeout if `null`." );
      ( "write_timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some "Write timeout. Defaults to harbor's timeout if `null`." );
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
           private key. Read when the first port opens and on `reload()`. \
           Unused in client mode." );
      ( "client_certificate",
        Lang.getter_t (Lang.nullable_t Lang.string_t),
        Some Lang.null,
        Some
          "Path to client certificate file. If passed in server mode, clients \
           will be required to present a certificate from this file. If passed \
           in client mode, the first certificate in this file will be \
           presented to the server. In server mode, read when the first port \
           opens and on `reload()`." );
      ( "client_key",
        Lang.getter_t (Lang.nullable_t Lang.string_t),
        Some Lang.null,
        Some
          "Path to client certificate private key. Required in client mode if \
           a client certificate is passed. Unused in server mode." );
    ]
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
      let path name =
        Lang.to_valued_option Lang.to_string
          (Lang.to_getter (List.assoc name p) ())
      in
      let find name () =
        match path name with
          | None ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ("Cannot find " ^ name ^ "file!")
          | Some path -> Utils.check_readable ~pos:(Lang.pos p) path
      in
      let find_opt name () =
        match path name with
          | None -> None
          | Some path -> Some (Utils.check_readable ~pos:(Lang.pos p) path)
      in
      let certificate = find "certificate" in
      let key = find_opt "key" in
      let client_certificate = find_opt "client_certificate" in
      let client_key = find "client_key" in
      ( server_config ~certificate ~key ~client_certificate,
        fun server_config ->
          Lang.http_transport
            (transport ~read_timeout ~write_timeout ~server_config ~certificate
               ~client_certificate ~client_key ()) ))
