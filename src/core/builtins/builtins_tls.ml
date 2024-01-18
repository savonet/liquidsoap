(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

module Liq_tls = struct
  type t = {
    read_pending : Buffer.t;
    fd : Unix.file_descr;
    buf : bytes;
    mutable state : Tls.Engine.state;
  }

  let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
  let buf_len = 4096
  let write_all fd data = Utils.write_all fd (Cstruct.to_bytes data)

  let read h len =
    let n = Unix.read h.fd h.buf 0 (min len buf_len) in
    Cstruct.of_bytes ~len:n h.buf

  let read_pending h = function
    | None -> ()
    | Some data -> Buffer.add_string h.read_pending (Cstruct.to_string data)

  let write_response h = function
    | None -> ()
    | Some data -> write_all h.fd data

  let handshake h =
    let rec f () =
      if Tls.Engine.handshake_in_progress h.state then (
        match Tls.Engine.handle_tls h.state (read h buf_len) with
          | Ok (`Eof, _, _) ->
              Runtime_error.raise ~pos:[]
                ~message:"Connection closed while negotiating TLS handshake!"
                "tls"
          | Ok ((`Ok _ as step), `Response response, `Data data)
          | Ok ((`Alert _ as step), `Response response, `Data data) ->
              read_pending h data;
              write_response h response;
              (match step with
                | `Ok state -> h.state <- state
                | `Alert alert ->
                    Runtime_error.raise ~pos:[]
                      ~message:
                        (Printf.sprintf "TLS handshake error: %s"
                           (Tls.Packet.alert_type_to_string alert))
                      "tls");
              f ()
          | Error (error, `Response response) ->
              write_all h.fd response;
              Runtime_error.raise ~pos:[]
                ~message:
                  (Printf.sprintf "TLS handshake error: %s"
                     (Tls.Packet.alert_type_to_string
                        (Tls.Engine.alert_of_failure error)))
                "tls")
    in
    f ()

  let init_base ~state fd =
    let buf = Bytes.create buf_len in
    let read_pending = Buffer.create 4096 in
    let h = { read_pending; fd; buf; state } in
    handshake h;
    h

  let init_server ~server fd =
    let state = Tls.Engine.server server in
    init_base ~state fd

  let init_client ~client fd =
    let state, hello = Tls.Engine.client client in
    write_all fd hello;
    init_base ~state fd

  let write h b off len =
    match
      Tls.Engine.send_application_data h.state [Cstruct.of_bytes ~off ~len b]
    with
      | None -> len
      | Some (state, data) ->
          write_all h.fd data;
          h.state <- state;
          len

  let read h b off len =
    let pending = Buffer.length h.read_pending in
    if 0 < pending then (
      let n = min pending len in
      Buffer.blit h.read_pending 0 b off n;
      Utils.buffer_drop h.read_pending n;
      n)
    else (
      let rec f () =
        match Tls.Engine.handle_tls h.state (read h len) with
          | Ok (`Eof, _, _) -> 0
          | Ok (`Alert alert, `Response response, _) ->
              ignore (Option.map (write_all h.fd) response);
              Runtime_error.raise ~pos:[]
                ~message:
                  (Printf.sprintf "TLS read error: %s"
                     (Tls.Packet.alert_type_to_string alert))
                "tls"
          | Ok (`Ok state, `Response response, `Data data) -> (
              ignore (Option.map (write_all h.fd) response);
              h.state <- state;
              match data with
                | None -> f ()
                | Some data ->
                    let data_len = Cstruct.length data in
                    let n = min data_len len in
                    Cstruct.blit_to_bytes data 0 b off n;
                    if n < data_len then
                      Buffer.add_string h.read_pending
                        (Cstruct.to_string data ~off:n ~len:(data_len - n));
                    n)
          | Error (error, `Response response) ->
              write_all h.fd response;
              Runtime_error.raise ~pos:[]
                ~message:
                  (Printf.sprintf "TLS read error: %s"
                     (Tls.Packet.alert_type_to_string
                        (Tls.Engine.alert_of_failure error)))
                "tls"
      in
      f ())

  let close h =
    let state, data = Tls.Engine.send_close_notify h.state in
    write_all h.fd data;
    h.state <- state;
    Unix.close h.fd
end

let tls_socket ~session transport =
  object
    method typ = "tls"
    method transport = transport
    method file_descr = session.Liq_tls.fd

    method wait_for ?log event timeout =
      let event =
        match event with
          | `Read -> `Read session.Liq_tls.fd
          | `Write -> `Write session.Liq_tls.fd
          | `Both -> `Both session.Liq_tls.fd
      in
      Tutils.wait_for ?log event timeout

    method read = Liq_tls.read session
    method write = Liq_tls.write session
    method close = Liq_tls.close session
  end

let server ~read_timeout ~write_timeout ~certificate ~key transport =
  let server =
    try
      let certificate = Cstruct.of_string (Utils.read_all (certificate ())) in
      let certificates =
        Result.get_ok (X509.Certificate.decode_pem_multiple certificate)
      in
      let key =
        Result.get_ok
          (X509.Private_key.decode_pem
             (Cstruct.of_string (Utils.read_all (key ()))))
      in
      Tls.Config.server ~certificates:(`Single (certificates, key)) ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Lang.raise_as_runtime ~bt ~kind:"tls" exn
  in
  object
    method transport = transport

    method accept ?timeout sock =
      let fd, caller = Http.accept ?timeout sock in
      try
        (match timeout with
          | Some timeout ->
              Http.set_socket_default ~read_timeout:timeout
                ~write_timeout:timeout fd
          | None -> ());
        let session = Liq_tls.init_server ~server fd in
        Http.set_socket_default ~read_timeout ~write_timeout fd;
        (tls_socket ~session transport, caller)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Unix.close fd;
        Printexc.raise_with_backtrace exn bt
  end

let transport ~read_timeout ~write_timeout ~certificate ~key () =
  object (self)
    method name = "tls"
    method protocol = "https"
    method default_port = 443

    method connect ?bind_address ?timeout ?prefer host port =
      let domain = Domain_name.host_exn (Domain_name.of_string_exn host) in
      let authenticator = Result.get_ok (Ca_certs.authenticator ()) in
      let certificate_authenticator =
        try
          let certificates =
            Result.get_ok
              (X509.Certificate.decode_pem_multiple
                 (Cstruct.of_string (Utils.read_all (certificate ()))))
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
      let client = Tls.Config.client ~authenticator ~peer_name:domain () in
      let fd = Http.connect ?bind_address ?timeout ?prefer host port in
      let session = Liq_tls.init_client ~client fd in
      tls_socket ~session self

    method server = server ~read_timeout ~write_timeout ~certificate ~key self
  end

let _ =
  Lang.add_builtin ~base:Modules.http_transport "tls" ~category:`Internet
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
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Path to certificate file. Required in server mode, e.g. \
           `input.harbor`, etc. If passed in client mode, certificate is added \
           to the list of valid certificates." );
      ( "key",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Path to certificate private key. Required in server mode, e.g. \
           `input.harbor`, etc. Unused in client mode." );
    ]
    Lang.http_transport_t
    (fun p ->
      let read_timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "read_timeout" p)
      in
      let read_timeout =
        Option.value ~default:Harbor_base.conf_timeout#get read_timeout
      in
      let write_timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "write_timeout" p)
      in
      let write_timeout =
        Option.value ~default:Harbor_base.conf_timeout#get write_timeout
      in
      let find name () =
        match Lang.to_valued_option Lang.to_string (List.assoc name p) with
          | None ->
              Runtime_error.raise ~pos:(Lang.pos p) "Cannot find "
              ^ name ^ "file!"
          | Some path -> Utils.check_readable ~pos:(Lang.pos p) path
      in
      let certificate = find "certificate" in
      let key = find "key" in
      Lang.http_transport
        (transport ~read_timeout ~write_timeout ~certificate ~key ()))
