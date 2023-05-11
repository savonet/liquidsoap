(* -*- mode: tuareg; -*- *)
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

let set_socket_default ~read_timeout ~write_timeout fd =
  ignore (Option.map (Unix.setsockopt_float fd Unix.SO_RCVTIMEO) read_timeout);
  ignore (Option.map (Unix.setsockopt_float fd Unix.SO_SNDTIMEO) write_timeout)

let ssl_socket transport ssl =
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

    method close =
      ignore (Ssl.close_notify ssl);
      Unix.close (Ssl.file_descr_of_socket ssl)
  end

let server ~read_timeout ~write_timeout ~password ~certificate ~key transport =
  let context = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
  let () =
    ignore
      (Option.map
         (fun password -> Ssl.set_password_callback context (fun _ -> password))
         password);
    Ssl.use_certificate context (certificate ()) (key ())
  in
  object
    method transport = transport

    method accept sock =
      let s, caller = Unix.accept ~cloexec:true sock in
      set_socket_default ~read_timeout ~write_timeout s;
      let ssl_s = Ssl.embed_socket s context in
      Ssl.accept ssl_s;
      (ssl_socket transport ssl_s, caller)
  end

let transport ~read_timeout ~write_timeout ~password ~certificate ~key () =
  object (self)
    method name = "ssl"
    method protocol = "https"
    method default_port = 443

    method connect ?bind_address ?timeout host port =
      try
        let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
        (* TODO: add option.. *)
        Ssl.set_verify ctx [] (Some Ssl.client_verify_callback);
        (* Add certificate from transport if passed. *)
        (try
           let cert = Utils.read_all (certificate ()) in
           Ssl.add_cert_to_store ctx cert
         with _ -> ());
        Ssl.set_verify_depth ctx 3;
        ignore (Ssl.set_default_verify_paths ctx);
        let unix_socket = Http.connect ?bind_address ?timeout host port in
        let socket =
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
            socket
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Unix.close unix_socket;
            Printexc.raise_with_backtrace exn bt
        in
        begin
          match bind_address with
            | None -> ()
            | Some s ->
                let unix_socket = Ssl.file_descr_of_socket socket in
                let bind_addr_inet =
                  (Unix.gethostbyname s).Unix.h_addr_list.(0)
                in
                (* Seems like you need to bind on port 0 *)
                let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
                Unix.bind unix_socket bind_addr
        end;
        ssl_socket self socket
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"ssl" exn

    method server =
      server ~read_timeout ~write_timeout ~password ~certificate ~key self
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
      let write_timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "write_timeout" p)
      in
      let password =
        Lang.to_valued_option Lang.to_string (List.assoc "password" p)
      in
      let raise name =
        Runtime_error.raise ~pos:(Lang.pos p)
          ~message:("Cannot find SSL " ^ name ^ " file!")
          "not_found"
      in
      let find name () =
        match Lang.to_valued_option Lang.to_string (List.assoc name p) with
          | None -> raise name
          | Some f when not (Sys.file_exists f) -> raise name
          | Some f -> f
      in
      let certificate = find "certificate" in
      let key = find "key" in
      Lang.http_transport
        (transport ~read_timeout ~write_timeout ~password ~certificate ~key ()))
