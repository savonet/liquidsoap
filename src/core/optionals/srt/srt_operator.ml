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

(** What both operators share (spec/common.md): the parameters and their
    validation, the role's callbacks and the script methods. Each operator calls
    [proto] for its prototype, [parse] when built, then hands [meth] and
    [callbacks] to [Lang.add_operator]. *)

let socket_with_methods = Srt_socket_value.to_value

let listen_callback_t =
  Lang.fun_t
    [
      (false, "hs_version", Lang.int_t);
      (false, "peeraddr", Lang.string_t);
      (false, "streamid", Lang.nullable_t Lang.string_t);
      (false, "", Srt_socket_value.t);
    ]
    Lang.bool_t

let proto ~mode =
  let string_default = function
    | `Listener -> Lang.string "listener"
    | `Caller -> Lang.string "caller"
  in
  [
    ( "mode",
      Lang.string_t,
      Some (string_default mode),
      Some "Connection mode. One of: `\"listener\"` or `\"caller\"`." );
    ( "host",
      Lang.string_t,
      Some (Lang.string "localhost"),
      Some "Address to connect to. Used only in caller mode." );
    ( "port",
      Lang.int_t,
      Some (Lang.int 8000),
      Some "Port to bind in listener mode, or to connect to in caller mode." );
    ( "bind_address",
      Lang.string_t,
      Some (Lang.string "0.0.0.0"),
      Some "Local address to bind. Used only in listener mode." );
    ( "prefer_address",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "Address family used when resolving hostnames. One of: `\"system\"`, \
         `\"ipv4\"` or `\"ipv6\"`. Defaults to `settings.srt.prefer_address` \
         when `null`." );
    ( "ipv6only",
      Lang.nullable_t Lang.bool_t,
      Some Lang.null,
      Some
        "Accept only IPv6 connections. Defaults to `true` when the bind \
         address is IPv6. Used only in listener mode." );
    ( "listen_callback",
      Lang.nullable_t listen_callback_t,
      Some Lang.null,
      Some
        "Admission predicate called during each handshake with the handshake \
         version, the peer's address, its stream id and the new socket: a \
         connection is admitted when it returns `true`. Used only in listener \
         mode. It runs on libsrt's single handshake thread: a slow callback \
         delays every handshake of the process." );
    ( "streamid",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some "Stream id sent in the handshake. Used only in caller mode." );
    ( "passphrase",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some
        "Encryption passphrase, 10 to 79 characters. `null` or `\"\"` means no \
         encryption." );
    ( "pbkeylen",
      Lang.nullable_t Lang.int_t,
      Some Lang.null,
      Some "Encryption key length. One of: 0 (libsrt's default), 16, 24 or 32."
    );
    ( "enforced_encryption",
      Lang.nullable_t Lang.bool_t,
      Some Lang.null,
      Some
        "Refuse connections whose encryption does not match. Defaults to \
         `settings.srt.enforced_encryption` when `null`." );
    ( "polling_delay",
      Lang.float_t,
      Some (Lang.float 2.),
      Some "Delay, in seconds, between two tries after a failure." );
    ( "connection_timeout",
      Lang.nullable_t Lang.float_t,
      Some Lang.null,
      Some
        "Connection timeout, in seconds. `null` keeps libsrt's default. Used \
         only in caller mode." );
    ( "read_timeout",
      Lang.nullable_t Lang.float_t,
      Some (Lang.float 1.),
      Some
        "Longest wait for data, in seconds, while the input paces its clock. \
         `null` means no limit. Used only by inputs." );
    ( "write_timeout",
      Lang.nullable_t Lang.float_t,
      Some (Lang.float 1.),
      Some
        "Longest time, in seconds, a peer may accept no data before it is \
         dropped. `null` means no limit. Used only by outputs." );
    ( "payload_size",
      Lang.int_t,
      Some (Lang.int 1316),
      Some "libsrt payload size, and the size of every message an output sends."
    );
    ( "messageapi",
      Lang.bool_t,
      Some (Lang.bool true),
      Some "libsrt message API flag. Live mode requires `true`." );
  ]

let invalid value message = raise (Error.Invalid_value (value, message, []))

let check_positive p name =
  let value = List.assoc name p in
  match Lang.to_valued_option Lang.to_float value with
    | Some v when v <= 0. -> invalid value "Value must be positive!"
    | v -> v

let valid_addresses = "`\"system\"`, `\"ipv4\"` or `\"ipv6\"`"

let parse_prefer_address p =
  let parse ~error s =
    match s with
      | "system" -> `System
      | "ipv4" -> `Ipv4
      | "ipv6" -> `Ipv6
      | _ -> error ()
  in
  let value = List.assoc "prefer_address" p in
  match Lang.to_valued_option Lang.to_string value with
    | Some s ->
        parse s ~error:(fun () ->
            invalid value ("Valid values are: " ^ valid_addresses ^ "."))
    | None ->
        let setting = Srt_lib.conf_prefer_address#get in
        parse setting ~error:(fun () ->
            invalid (Lang.string setting)
              ("Invalid `settings.srt.prefer_address`! Valid values are: "
             ^ valid_addresses ^ "."))

let parse_listen_callback p =
  Option.map
    (fun fn ~hs_version ~peeraddr ~streamid socket ->
      Lang.to_bool
        (Lang.apply fn
           [
             ("hs_version", Lang.int hs_version);
             ("peeraddr", Lang.string peeraddr);
             ( "streamid",
               match streamid with Some s -> Lang.string s | None -> Lang.null
             );
             ("", socket_with_methods socket);
           ]))
    (Lang.to_option (List.assoc "listen_callback" p))

(** The role's parameters, with [read_timeout] and [write_timeout], validated
    (spec/common.md §1). *)
let parse p =
  let get name f = f (List.assoc name p) in
  let mode =
    match get "mode" Lang.to_string with
      | "listener" -> `Listener
      | "caller" -> `Caller
      | _ ->
          invalid (List.assoc "mode" p)
            "Invalid mode! Should be one of: `\"listener\"` or `\"caller\"`."
  in
  let port = get "port" Lang.to_int in
  if port < 1 || port > 65535 then
    invalid (List.assoc "port" p) "Port must be between 1 and 65535!";
  let payload_size = get "payload_size" Lang.to_int in
  if payload_size < 1 || payload_size > Srt.live_max_payload_size then
    invalid
      (List.assoc "payload_size" p)
      (Printf.sprintf "Payload size must be between 1 and %d!"
         Srt.live_max_payload_size);
  let pbkeylen = get "pbkeylen" (Lang.to_valued_option Lang.to_int) in
  (match pbkeylen with
    | Some k when not (List.mem k [0; 16; 24; 32]) ->
        invalid (List.assoc "pbkeylen" p)
          "Key length must be one of: 0, 16, 24 or 32!"
    | _ -> ());
  let passphrase = get "passphrase" (Lang.to_valued_option Lang.to_string) in
  (match passphrase with
    | Some s when s <> "" && String.length s < 10 ->
        invalid
          (List.assoc "passphrase" p)
          "Passphrase must be at least 10 characters long!"
    | Some s when String.length s > 79 ->
        invalid
          (List.assoc "passphrase" p)
          "Passphrase must be at most 79 characters long!"
    | _ -> ());
  let messageapi = get "messageapi" Lang.to_bool in
  if not messageapi then
    invalid
      (List.assoc "messageapi" p)
      "Live mode only carries messages: must be true!";
  let polling_delay = Option.get (check_positive p "polling_delay") in
  let connection_timeout = check_positive p "connection_timeout" in
  let read_timeout = check_positive p "read_timeout" in
  let write_timeout = check_positive p "write_timeout" in
  let params =
    {
      Srt_connection.mode;
      host = get "host" Lang.to_string;
      port;
      bind_address = get "bind_address" Lang.to_string;
      prefer_address = parse_prefer_address p;
      ipv6only = get "ipv6only" (Lang.to_valued_option Lang.to_bool);
      listen_callback = parse_listen_callback p;
      streamid = get "streamid" (Lang.to_valued_option Lang.to_string);
      passphrase;
      pbkeylen;
      enforced_encryption =
        get "enforced_encryption" (Lang.to_valued_option Lang.to_bool);
      polling_delay;
      connection_timeout;
      payload_size;
      messageapi;
    }
  in
  (params, read_timeout, write_timeout)

let meth () =
  Lang.
    [
      {
        name = "sockets";
        scheme = ([], fun_t [] (list_t (product_t string_t Srt_socket_value.t)));
        descr = "The connected peers, with their IP address.";
        value =
          (fun s ->
            val_fun [] (fun _ ->
                list
                  (List.map
                     (fun (address, socket) ->
                       product (string address) (socket_with_methods socket))
                     s#connection#sockets)));
      };
      {
        name = "connect";
        scheme = ([], fun_t [] unit_t);
        descr = "Connect, or listen, when started.";
        value =
          (fun s ->
            val_fun [] (fun _ ->
                s#connection#connect;
                unit));
      };
      {
        name = "disconnect";
        scheme = ([], fun_t [] unit_t);
        descr = "Disconnect every peer and stop connecting until `connect`.";
        value =
          (fun s ->
            val_fun [] (fun _ ->
                s#connection#disconnect;
                unit));
      };
    ]
  @ Start_stop.meth ()

let callbacks () =
  Lang_source.
    [
      {
        name = "on_connect";
        params = [];
        descr = "when a peer connects, before any data is exchanged with it";
        register_deprecated_argument = true;
        arg_t = [];
        register =
          (fun ~params:_ s f ->
            s#connection#register_on_connect (fun () -> f []));
      };
      {
        name = "on_disconnect";
        params = [];
        descr = "when a peer that connected is disconnected";
        register_deprecated_argument = true;
        arg_t = [];
        register =
          (fun ~params:_ s f ->
            s#connection#register_on_disconnect (fun () -> f []));
      };
      {
        name = "on_socket";
        params = [];
        descr =
          "when a socket is created (mode `\"connect\"` or `\"listen\"`), \
           accepted (mode `\"incoming\"`), or about to be closed (mode \
           `\"close\"`)";
        register_deprecated_argument = true;
        arg_t =
          [(false, "mode", Lang.string_t); (false, "", Srt_socket_value.t)];
        register =
          (fun ~params:_ s f ->
            s#connection#register_on_socket (fun mode socket ->
                f [("mode", Lang.string mode); ("", socket_with_methods socket)]));
      };
    ]
