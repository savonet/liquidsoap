(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let log = Log.make ["ssl"]

module Http = Liq_http

let protocol_of_value protocol_val =
  match Lang.to_string protocol_val with
    | "ssl.3" -> Ssl.SSLv3 [@alert "-deprecated"]
    | "tls.1" -> Ssl.TLSv1 [@alert "-deprecated"]
    | "tls.1.1" -> Ssl.TLSv1_1 [@alert "-deprecated"]
    | "tls.1.2" -> Ssl.TLSv1_2
    | "tls.1.3" -> Ssl.TLSv1_3
    | _ -> raise (Error.Invalid_value (protocol_val, "Invalid SSL protocol"))

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

      method read = Ssl.read ssl
      method write = Ssl.write ssl
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

let server ~min_protocol ~max_protocol ~read_timeout ~write_timeout ~password
    ~certificate ~key transport =
  let context =
    Ssl.create_context (Ssl.SSLv23 [@alert "-deprecated"]) Ssl.Server_context
  in
  let () =
    ignore (Option.map (Ssl.set_min_protocol_version context) min_protocol)
  in
  let () =
    ignore (Option.map (Ssl.set_max_protocol_version context) max_protocol)
  in
  let () =
    ignore
      (Option.map
         (fun password -> Ssl.set_password_callback context (fun _ -> password))
         password);
    Ssl.use_certificate context (certificate ()) (key ())
  in
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
        let ssl_s = Ssl.embed_socket s context in
        Ssl.accept ssl_s;
        Http.set_socket_default ~read_timeout ~write_timeout s;
        (ssl_socket ~pos:[] transport ssl_s, caller)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Unix.close s;
        Printexc.raise_with_backtrace exn bt
  end

let transport ~min_protocol ~max_protocol ~read_timeout ~write_timeout ~password
    ~certificate ~key () =
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
        let () =
          ignore (Option.map (Ssl.set_min_protocol_version ctx) min_protocol)
        in
        let () =
          ignore (Option.map (Ssl.set_max_protocol_version ctx) max_protocol)
        in
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
                (Printf.sprintf "SSL verification error: %s"
                   (Ssl.get_verify_error_string err))
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
      server ~min_protocol ~max_protocol ~read_timeout ~write_timeout ~password
        ~certificate ~key self
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
           `input.harbor`, etc." );
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
        (transport ~min_protocol ~max_protocol ~read_timeout ~write_timeout
           ~password ~certificate ~key ()))
