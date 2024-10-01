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

module Http = Liq_http

module Socket_domain = struct
  include Value.MkCustom (struct
    type content = Unix.socket_domain

    let name = "socket_domain"

    let to_json ~pos _ =
      Lang.raise_error ~pos
        ~message:"Socket domain cannot be represented as json" "json"

    let to_string = function
      | Unix.PF_UNIX -> "socket.domain.unix"
      | Unix.PF_INET -> "socket.domain.inet"
      | Unix.PF_INET6 -> "socket.domain.inet6"

    let compare = Stdlib.compare
  end)

  let unix = to_value Unix.PF_UNIX
  let inet = to_value Unix.PF_INET
  let inet6 = to_value Unix.PF_INET6
end

module Socket_type = struct
  include Value.MkCustom (struct
    type content = Unix.socket_type

    let name = "socket_type"

    let to_json ~pos _ =
      Lang.raise_error ~pos ~message:"Socket type cannot be represented as json"
        "json"

    let to_string = function
      | Unix.SOCK_STREAM -> "socket.type.stream"
      | Unix.SOCK_DGRAM -> "socket.type.dgram"
      | Unix.SOCK_RAW -> "socket.type.raw"
      (* From OCaml: SOCK_SEQPACKET is included for completeness,
         but is rarely supported by the OS, and needs system calls
         that are not available in this library. *)
      | Unix.SOCK_SEQPACKET -> assert false

    let compare = Stdlib.compare
  end)

  let stream = to_value Unix.SOCK_STREAM
  let dgram = to_value Unix.SOCK_DGRAM
  let raw = to_value Unix.SOCK_RAW
end

module Inet_addr = struct
  let to_string s =
    Printf.sprintf "socket.internet_address(%S)" (Unix.string_of_inet_addr s)

  include Value.MkCustom (struct
    type content = Unix.inet_addr

    let name = "internet_address"

    let to_json ~pos _ =
      Lang.raise_error ~pos
        ~message:"Internet address type cannot be represented as json" "json"

    let to_string = to_string

    let compare s s' =
      Stdlib.compare (Unix.string_of_inet_addr s) (Unix.string_of_inet_addr s')
  end)

  let base_t = t

  let t =
    Lang.method_t base_t
      [
        ( "to_string",
          ([], Lang.fun_t [] Lang.string_t),
          "String representation of the internet address" );
        ("is_ipv6", ([], Lang.bool_t), "Is the internet address a ipv6 address?");
      ]

  let to_value v =
    Lang.meth (to_value v)
      [
        ( "to_string",
          Lang.val_fun [] (fun _ -> Lang.string (Unix.string_of_inet_addr v)) );
        ("is_ipv6", Lang.bool (Unix.is_inet6_addr v));
      ]

  let any = to_value Unix.inet_addr_any
  let loopback = to_value Unix.inet_addr_loopback
  let ipv6_any = to_value Unix.inet6_addr_any
  let ipv6_loopback = to_value Unix.inet6_addr_loopback
end

module Socket_addr = struct
  include Value.MkCustom (struct
    type content = Unix.sockaddr

    let name = "socket_address"

    let to_json ~pos _ =
      Lang.raise_error ~pos
        ~message:"Socket address type cannot be represented as json" "json"

    let to_string = function
      | Unix.ADDR_UNIX s -> Printf.sprintf "socket.address.unix(%S)" s
      | Unix.ADDR_INET (inet_addr, port) ->
          Printf.sprintf "socket.address.inet(%s, %i)"
            (Inet_addr.to_string inet_addr)
            port

    let compare = Stdlib.compare
  end)

  let base_t = t

  let t =
    Lang.method_t base_t [("domain", ([], Socket_domain.t), "Socket domain")]

  let to_value v =
    Lang.meth (to_value v)
      [("domain", Socket_domain.to_value (Unix.domain_of_sockaddr v))]

  let unix_t =
    Lang.method_t t [("path", ([], Lang.string_t), "Unix socket path")]

  let inet_t =
    Lang.method_t t
      [
        ("internet_address", ([], Inet_addr.t), "Internet address");
        ("port", ([], Lang.int_t), "Port");
      ]

  let to_unix_value = function
    | Unix.ADDR_UNIX path as v ->
        Lang.meth (to_value v) [("path", Lang.string path)]
    | _ -> assert false

  let to_inet_value = function
    | Unix.ADDR_INET (addr, port) as v ->
        Lang.meth (to_value v)
          [
            ("internet_address", Inet_addr.to_value addr);
            ("port", Lang.int port);
          ]
    | _ -> assert false
end

module Socket_value = struct
  include Value.MkCustom (struct
    type content = Http.socket

    let name = "socket"

    let to_json ~pos _ =
      Lang.raise_error ~pos ~message:"Socket cannot be represented as json"
        "json"

    let to_string s = Printf.sprintf "<%s socket>" s#typ
    let compare = Stdlib.compare
  end)

  let meths =
    let string_of_mode = function `Read -> "read" | `Write -> "write" in
    let wait_t ~mode t =
      Lang.method_t t
        [
          ( "wait",
            ( [],
              Lang.fun_t
                [
                  (true, "timeout", Lang.nullable_t Lang.float_t);
                  (false, "", Lang.fun_t [] Lang.unit_t);
                ]
                Lang.unit_t ),
            "Execute the given callback when the socket is ready to "
            ^ string_of_mode mode ^ " some data" );
        ]
    in
    let wait_meth ~mode socket v =
      Lang.meth v
        [
          ( "wait",
            Lang.val_fun
              [("timeout", "timeout", Some (Lang.float 10.)); ("", "", None)]
              (fun p ->
                let timeout =
                  Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
                in
                let event =
                  match mode with
                    | `Read -> `Read socket#file_descr
                    | `Write -> `Write socket#file_descr
                in
                let events =
                  match timeout with
                    | None -> [event]
                    | Some t -> [`Delay t; event]
                in
                let fn = List.assoc "" p in
                let fn events =
                  if not (List.mem event events) then
                    Lang.raise_error ~pos:(Lang.pos p)
                      ~message:"Timeout while writing to the socket!" "socket";
                  ignore (Lang.apply fn []);
                  []
                in
                Duppy.Task.add Tutils.scheduler
                  { Duppy.Task.priority = `Generic; events; handler = fn };
                Lang.unit) );
        ]
    in
    [
      ( "type",
        ([], Lang.string_t),
        "Socket type",
        fun (socket : content) -> Lang.string socket#typ );
      ( "non_blocking",
        ([], Lang.fun_t [(false, "", Lang.bool_t)] Lang.unit_t),
        "Set the non-blocking flag on the socket",
        fun socket ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              if Lang.to_bool (List.assoc "" p) then
                Unix.set_nonblock socket#file_descr
              else Unix.clear_nonblock socket#file_descr;
              Lang.unit) );
      ( "write",
        ( [],
          wait_t ~mode:`Write
            (Lang.fun_t
               [
                 (true, "timeout", Lang.nullable_t Lang.float_t);
                 (false, "", Lang.string_t);
               ]
               Lang.unit_t) ),
        "Write data to a socket",
        fun socket ->
          wait_meth ~mode:`Write socket
            (Lang.val_fun
               [("timeout", "timeout", Some (Lang.float 10.)); ("", "", None)]
               (fun p ->
                 let timeout =
                   Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
                 in
                 let data = Lang.to_string (List.assoc "" p) in
                 let data = Bytes.of_string data in
                 let len = Bytes.length data in
                 let start_time = Unix.gettimeofday () in
                 let check_timeout () =
                   match timeout with
                     | None -> ()
                     | Some t -> (
                         let rem = start_time +. t -. Unix.gettimeofday () in
                         try
                           if rem <= 0. then failwith "timeout!";
                           socket#wait_for `Write rem
                         with _ ->
                           Lang.raise_error ~pos:(Lang.pos p)
                             ~message:"Timeout while writing to the socket!"
                             "socket")
                 in
                 try
                   let rec f pos =
                     check_timeout ();
                     let n = socket#write data pos (len - pos) in
                     if n < len then f (pos + n)
                   in
                   f 0;
                   Lang.unit
                 with exn ->
                   let bt = Printexc.get_raw_backtrace () in
                   Lang.raise_as_runtime ~bt ~kind:"socket" exn)) );
      ( "read",
        ( [],
          wait_t ~mode:`Read
            (Lang.fun_t
               [(true, "timeout", Lang.nullable_t Lang.float_t)]
               Lang.string_t) ),
        "Read data from a socket. Reading is done when the function returns an \
         empty string `\"\"`.",
        fun socket ->
          let buflen = Utils.pagesize in
          let buf = Bytes.create buflen in
          wait_meth ~mode:`Read socket
            (Lang.val_fun
               [("timeout", "timeout", Some (Lang.float 10.))]
               (fun p ->
                 let timeout =
                   Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
                 in
                 let start_time = Unix.gettimeofday () in
                 let check_timeout () =
                   match timeout with
                     | None -> ()
                     | Some t -> (
                         let rem = start_time +. t -. Unix.gettimeofday () in
                         try
                           if rem <= 0. then failwith "timeout!";
                           socket#wait_for `Read rem
                         with _ ->
                           Lang.raise_error ~pos:(Lang.pos p)
                             ~message:"Timeout while reading from the socket!"
                             "socket")
                 in
                 try
                   check_timeout ();
                   let n = socket#read buf 0 buflen in
                   Lang.string (Bytes.sub_string buf 0 n)
                 with exn ->
                   let bt = Printexc.get_raw_backtrace () in
                   Lang.raise_as_runtime ~bt ~kind:"socket" exn)) );
      ( "close",
        ([], Lang.fun_t [] Lang.unit_t),
        "Close the socket.",
        fun socket ->
          Lang.val_fun [] (fun _ ->
              try
                socket#close;
                Lang.unit
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
    ]

  let t =
    Lang.method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value v =
    Lang.meth (to_value v) (List.map (fun (lbl, _, _, m) -> (lbl, m v)) meths)

  let server_t =
    Lang.method_t t
      [
        ( "bind",
          ([], Lang.fun_t [(false, "", Socket_addr.base_t)] Lang.unit_t),
          "Bind a socket to an address." );
        ( "listen",
          ([], Lang.fun_t [(false, "", Lang.int_t)] Lang.unit_t),
          "Set up a socket for receiving connection requests. The integer \
           argument is the maximal number of pending requests." );
      ]

  let to_server_value socket =
    Lang.meth (to_value socket)
      [
        ( "bind",
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              Unix.bind socket#file_descr
                (Socket_addr.of_value (List.assoc "" p));
              Lang.unit) );
        ( "listen",
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              Unix.listen socket#file_descr (Lang.to_int (List.assoc "" p));
              Lang.unit) );
      ]

  let unix_t =
    Lang.method_t server_t
      [
        ( "accept",
          ( [],
            Lang.fun_t
              [(true, "timeout", Lang.nullable_t Lang.float_t)]
              (Lang.product_t t Socket_addr.base_t) ),
          "Accept connections on the given socket. The returned socket is a \
           socket connected to the client; the returned address is the address \
           of the connecting client. Timeout defaults to harbor's \
           accept_timeout if `null`." );
        ( "connect",
          ([], Lang.fun_t [(false, "", Socket_addr.base_t)] Lang.unit_t),
          "Connect a socket to an address." );
      ]

  let to_unix_value v =
    let socket = Http.unix_socket v in
    let server = socket#transport#server in
    Lang.meth (to_server_value socket)
      [
        ( "accept",
          Lang.val_fun
            [
              ( "timeout",
                "timeout",
                Some (Lang.float Harbor_base.conf_accept_timeout#get) );
            ]
            (fun p ->
              let timeout =
                Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
              in
              let fd, sockaddr = server#accept ?timeout socket#file_descr in
              Lang.product (to_value fd) (Socket_addr.to_value sockaddr)) );
        ( "connect",
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              Unix.connect socket#file_descr
                (Socket_addr.of_value (List.assoc "" p));
              Lang.unit) );
      ]
end

let socket = Lang.add_module "socket"

let _ =
  Lang.add_builtin ~base:socket "unix" ~category:`Internet
    [
      ("domain", Socket_domain.t, Some Socket_domain.inet, Some "Socket domain.");
      ("type", Socket_type.t, Some Socket_type.stream, Some "Socket type");
      ( "protocol",
        Lang.int_t,
        Some (Lang.int 0),
        Some
          "Protocol type. `0` selects the default protocol for that kind of \
           sockets." );
    ]
    Socket_value.unix_t ~descr:"Create a unix socket."
    (fun p ->
      let domain = Socket_domain.of_value (List.assoc "domain" p) in
      let typ = Socket_type.of_value (List.assoc "type" p) in
      let protocol = Lang.to_int (List.assoc "protocol" p) in
      Socket_value.to_unix_value (Unix.socket ~cloexec:true domain typ protocol))

let _ =
  Lang.add_builtin ~base:socket "pair" ~category:`Internet
    [
      ("domain", Socket_domain.t, Some Socket_domain.inet, Some "Socket domain.");
      ("type", Socket_type.t, Some Socket_type.stream, Some "Socket type");
      ( "protocol",
        Lang.int_t,
        Some (Lang.int 0),
        Some
          "Protocol type. `0` selects the default protocol for that kind of \
           sockets." );
    ]
    Socket_value.t ~descr:"Create a pair of sockets connected together."
    (fun p ->
      let domain = Socket_domain.of_value (List.assoc "domain" p) in
      let typ = Socket_type.of_value (List.assoc "type" p) in
      let protocol = Lang.to_int (List.assoc "protocol" p) in
      let s, s' = Unix.socketpair ~cloexec:true domain typ protocol in
      Lang.product
        (Socket_value.to_unix_value s)
        (Socket_value.to_unix_value s'))

let socket_domain = Lang.add_module ~base:socket "domain"
let socket_type = Lang.add_module ~base:socket "type"

let add ~t ~name ~descr ~base value =
  ignore (Lang.add_builtin_value ~category:`Internet ~descr ~base name value t)

let () =
  add ~t:Socket_domain.t ~base:socket_domain ~name:"unix"
    ~descr:"Unix socket domain" Socket_domain.unix;
  add ~t:Socket_domain.t ~base:socket_domain ~name:"inet"
    ~descr:"Inet socket domain" Socket_domain.inet;
  add ~t:Socket_domain.t ~base:socket_domain ~name:"inet6"
    ~descr:"Inet6 socket domain" Socket_domain.inet6;
  add ~t:Socket_type.t ~base:socket_type ~name:"stream"
    ~descr:"Stream socket type" Socket_type.stream;
  add ~t:Socket_type.t ~base:socket_type ~name:"dgram"
    ~descr:"Dgram socket type" Socket_type.dgram;
  add ~t:Socket_type.t ~base:socket_type ~name:"raw" ~descr:"Raw socket type"
    Socket_type.raw

let socket_internet_address =
  Lang.add_builtin ~base:socket "internet_address" ~category:`Internet
    [("", Lang.string_t, None, Some "Socket internet address.")]
    Inet_addr.t
    ~descr:"Return an internet address from its string representation."
    (fun p ->
      Inet_addr.to_value
        (Unix.inet_addr_of_string (Lang.to_string (List.assoc "" p))))

let () =
  add ~t:Inet_addr.t ~base:socket_internet_address ~name:"any"
    ~descr:
      "A special IPv4 address, for use only with `socket.bind`, representing \
       all the Internet addresses that the host machine possesses."
    Inet_addr.any;
  add ~t:Inet_addr.t ~base:socket_internet_address ~name:"loopback"
    ~descr:"A special IPv4 address representing the host machine (`127.0.0.1`)."
    Inet_addr.loopback

let socket_internet_address_ipv6 =
  Lang.add_module ~base:socket_internet_address "ipv6"

let () =
  add ~t:Inet_addr.t ~base:socket_internet_address_ipv6 ~name:"any"
    ~descr:
      "A special IPv6 address, for use only with `socket.bind`, representing \
       all the Internet addresses that the host machine possesses."
    Inet_addr.ipv6_any;
  add ~t:Inet_addr.t ~base:socket_internet_address_ipv6 ~name:"loopback"
    ~descr:"A special IPv6 address representing the host machine (`::1`)."
    Inet_addr.ipv6_loopback

let socket_address = Lang.add_module ~base:socket "address"

let socket_address_unix =
  Lang.add_builtin ~base:socket_address "unix" ~category:`Internet
    [("", Lang.string_t, None, Some "Unix socket path")]
    Socket_addr.unix_t ~descr:"Create a socket address for a unix file socket."
    (fun p ->
      Socket_addr.to_unix_value
        (Unix.ADDR_UNIX (Lang.to_string (List.assoc "" p))))

let _ =
  Lang.add_builtin ~base:socket_address "internet_address" ~category:`Internet
    [
      ("", Inet_addr.t, None, Some "Internet address.");
      ("", Lang.int_t, None, Some "port");
    ]
    Socket_addr.inet_t ~descr:"Create a socket address for a internet address."
    (fun p ->
      let addr = Inet_addr.of_value (Lang.assoc "" 1 p) in
      let port = Lang.to_int (Lang.assoc "" 2 p) in
      Socket_addr.to_inet_value (Unix.ADDR_INET (addr, port)))

let host_t =
  Lang.record_t
    [
      ("name", Lang.string_t);
      ("aliases", Lang.list_t Lang.string_t);
      ("domain", Socket_domain.t);
      ("addresses", Lang.list_t Inet_addr.t);
    ]

let to_host_value { Unix.h_name; h_aliases; h_addrtype; h_addr_list } =
  Lang.record
    [
      ("name", Lang.string h_name);
      ("aliases", Lang.list (List.map Lang.string (Array.to_list h_aliases)));
      ("domain", Socket_domain.to_value h_addrtype);
      ( "addresses",
        Lang.list (List.map Inet_addr.to_value (Array.to_list h_addr_list)) );
    ]

let host = Lang.add_module "host"

let _ =
  Lang.add_builtin ~base:host "of_name" ~category:`Internet
    ~descr:"Find a host by name"
    [("", Lang.string_t, None, Some "hostname")]
    (Lang.nullable_t host_t)
    (fun p ->
      let hostname = Lang.to_string (List.assoc "" p) in
      try to_host_value (Unix.gethostbyname hostname)
      with Not_found -> Lang.null)

let _ =
  Lang.add_builtin ~base:host "of_internet_address" ~category:`Internet
    ~descr:"Find a host by internet address"
    [("", Inet_addr.base_t, None, None)]
    (Lang.nullable_t host_t)
    (fun p ->
      let inet_addr = Inet_addr.of_value (List.assoc "" p) in
      try to_host_value (Unix.gethostbyaddr inet_addr)
      with Not_found -> Lang.null)
