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

let resp_t = Lang.nullable_t (Lang.getter_t Lang.string_t)
let harbor = Modules.harbor
let harbor_http = Lang.add_module ~base:harbor "http"

module Http = Liq_http

let request_t =
  Lang.record_t
    [
      ("http_version", Lang.string_t);
      ("method", Lang.string_t);
      ("data", Lang.fun_t [(true, "timeout", Lang.float_t)] Lang.string_t);
      ("headers", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
      ("query", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
      ("socket", Builtins_socket.Socket_value.t);
      ("path", Lang.string_t);
    ]

let register_args =
  [
    ("port", Lang.int_t, Some (Lang.int 8000), Some "Port to serve.");
    ( "transport",
      Lang.http_transport_t,
      Some (Lang.http_transport Http.unix_transport),
      Some
        "Http transport. Use `http.transport.ssl` or \
         `http.transport.secure_transport`, when available, to enable HTTPS \
         output" );
    ( "method",
      Lang.string_t,
      Some (Lang.string "GET"),
      Some
        "Accepted method (\"GET\" / \"POST\" / \"PUT\" / \"DELETE\" / \"HEAD\" \
         / \"OPTIONS\")." );
    ("", Lang.regexp_t, None, Some "path to to serve.");
    ( "",
      Lang.fun_t [(false, "", request_t)] resp_t,
      None,
      Some "Handler function" );
  ]

let parse_register_args p =
  let port = Lang.to_int (List.assoc "port" p) in
  let transport = Lang.to_http_transport (List.assoc "transport" p) in
  let verb = Harbor.verb_of_string (Lang.to_string (List.assoc "method" p)) in
  let uri = Lang.to_regexp (Lang.assoc "" 1 p) in
  let f = Lang.assoc "" 2 p in
  let handler ~protocol ~meth ~data ~headers ~query ~socket path =
    let meth = Harbor.string_of_verb meth in
    let headers =
      List.map
        (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
        headers
    in
    let headers = Lang.list headers in
    let query =
      List.map
        (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
        query
    in
    let query = Lang.list query in
    let socket =
      Builtins_socket.Socket_value.to_value
        (socket :> Builtins_socket.Socket_value.content)
    in
    let request =
      Lang.record
        [
          ("http_version", Lang.string protocol);
          ("method", Lang.string meth);
          ("headers", headers);
          ("query", query);
          ("socket", socket);
          ( "data",
            Lang.val_fun
              [("timeout", "timeout", Some (Lang.float 10.))]
              (fun p ->
                let timeout = Lang.to_float (List.assoc "timeout" p) in
                Lang.string (data timeout)) );
          ("path", Lang.string path);
        ]
    in

    let resp = Lang.apply f [("", request)] in

    match Lang.to_option resp with
      | None -> Harbor.custom ()
      | Some resp -> (
          try Harbor.simple_reply (Lang.to_string resp)
          with _ -> Harbor.reply (Lang.to_string_getter resp))
  in
  (uri, port, transport, verb, handler)

let _ =
  Lang.add_builtin ~base:harbor_http "register" ~category:`Internet
    ~descr:
      "Low-level harbor handler registration. Overridden in standard library."
    register_args Lang.unit_t (fun p ->
      let uri, port, transport, verb, handler = parse_register_args p in
      Harbor.add_http_handler ~pos:(Lang.pos p) ~transport ~port ~verb ~uri
        handler;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:harbor "remove" ~category:`Internet
    ~descr:(Printf.sprintf "Remove a registered handler on the harbor.")
    [
      ("port", Lang.int_t, Some (Lang.int 8000), Some "Port to serve.");
      ("method", Lang.string_t, Some (Lang.string "GET"), Some "Method served.");
      ("", Lang.regexp_t, None, Some "URI served.");
    ]
    Lang.unit_t
    (fun p ->
      let port = Lang.to_int (List.assoc "port" p) in
      let uri = Lang.to_regexp (Lang.assoc "" 1 p) in
      let verb =
        Harbor.verb_of_string (Lang.to_string (List.assoc "method" p))
      in
      Harbor.remove_http_handler ~port ~verb ~uri ();
      Lang.unit)
