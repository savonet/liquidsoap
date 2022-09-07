(* -*- mode: tuareg; -*- *)
(*****************************************************************************

    Liquidsoap, a programmable audio stream generator.
    Copyright 2003-2022 Savonet team

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

module type T = sig
  include Harbor.T

  val name : string
end

module Make (Harbor : T) = struct
  let name_up = String.uppercase_ascii Harbor.name
  let () = Lang.add_module ("harbor." ^ Harbor.name)

  let request_t =
    Lang.record_t
      [
        ("protocol", Lang.string_t);
        ("method", Lang.string_t);
        ("data", Lang.string_t);
        ("headers", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
        ("query", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
        ("socket", Builtins_socket.SocketValue.t);
        ("path", Lang.string_t);
      ]

  let response_t =
    let getter_setter_t ty =
      Lang.method_t
        (Lang.fun_t [(false, "", ty)] Lang.unit_t)
        [("current", ([], Lang.fun_t [] ty), "Get current value")]
    in
    Lang.record_t
      [
        ("protocol", getter_setter_t Lang.string_t);
        ("code", getter_setter_t Lang.int_t);
        ("status", getter_setter_t (Lang.nullable_t Lang.string_t));
        ("data", getter_setter_t (Lang.getter_t Lang.string_t));
        ("content_type", getter_setter_t (Lang.nullable_t Lang.string_t));
        ( "headers",
          getter_setter_t
            (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)) );
        ("custom", getter_setter_t Lang.bool_t);
      ]

  let register_args =
    [
      ("port", Lang.int_t, Some (Lang.int 8000), Some "Port to server.");
      ( "method",
        Lang.string_t,
        Some (Lang.string "GET"),
        Some
          "Accepted method (\"GET\" / \"POST\" / \"PUT\" / \"DELETE\" / \
           \"HEAD\" / \"OPTIONS\")." );
      ( "",
        Lang.fun_t [(false, "", request_t); (false, "", response_t)] Lang.unit_t,
        None,
        Some "Handler function" );
    ]

  let parse_register_args p =
    let port = Lang.to_int (List.assoc "port" p) in
    let verb = Harbor.verb_of_string (Lang.to_string (List.assoc "method" p)) in
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
        object
          method typ = Harbor.socket_type
          method read = Harbor.read socket
          method write = Harbor.write socket
          method close = Harbor.close socket
        end
      in
      let socket = Builtins_socket.SocketValue.to_value socket in
      let request =
        Lang.record
          [
            ("protocol", Lang.string protocol);
            ("method", Lang.string meth);
            ("headers", headers);
            ("query", query);
            ("socket", socket);
            ("data", Lang.string data);
            ("path", Lang.string path);
          ]
      in

      let resp_protocol = Atomic.make protocol in
      let resp_code = Atomic.make 200 in
      let resp_status = Atomic.make None in
      let resp_headers = Atomic.make [] in
      let resp_content_type = Atomic.make None in
      let resp_data = Atomic.make (fun () -> "") in
      let resp_custom = Atomic.make false in

      let getter_setter to_v of_v v =
        Lang.meth
          (Lang.val_fun [("", "", None)] (fun p ->
               Atomic.set v (of_v (List.assoc "" p));
               Lang.unit))
          [("current", Lang.val_fun [] (fun _ -> to_v (Atomic.get v)))]
      in

      let response =
        Lang.record
          [
            ("protocol", getter_setter Lang.string Lang.to_string resp_protocol);
            ("code", getter_setter Lang.int Lang.to_int resp_code);
            ( "status",
              getter_setter
                (function None -> Lang.null | Some s -> Lang.string s)
                Lang.(to_valued_option to_string)
                resp_status );
            ( "headers",
              getter_setter
                (fun headers ->
                  Lang.(
                    list
                      (List.map
                         (fun (v, v') -> product (string v) (string v'))
                         headers)))
                (fun headers ->
                  Lang.(
                    List.map
                      (fun v ->
                        let v, v' = to_product v in
                        (to_string v, to_string v'))
                      (to_list headers)))
                resp_headers );
            ( "content_type",
              getter_setter
                (function None -> Lang.null | Some s -> Lang.string s)
                Lang.(to_valued_option to_string)
                resp_content_type );
            ("custom", getter_setter Lang.bool Lang.to_bool resp_custom);
            ( "data",
              getter_setter
                (fun fn -> Lang.(val_fun [] (fun _ -> string (fn ()))))
                Lang.to_string_getter resp_data );
            ("custom", getter_setter Lang.bool Lang.to_bool resp_custom);
          ]
      in

      ignore (Lang.apply f [("", request); ("", response)]);
      Harbor.(
        http_reply
          {
            protocol = Atomic.get resp_protocol;
            code = Atomic.get resp_code;
            status = Atomic.get resp_status;
            headers =
              (Atomic.get resp_headers
              @
              match Atomic.get resp_content_type with
                | None -> []
                | Some m -> [("Content-Type", m)]);
            data = `String_getter (Atomic.get resp_data);
          })
    in
    (port, verb, handler)

  let () =
    Lang.add_builtin
      ("harbor." ^ Harbor.name ^ ".register")
      ~category:`Liquidsoap
      ~descr:
        [%string
          "Register a %{name_up} handler on the harbor. The handler function \
           receives as argument the full requested information and returns the \
           answer sent to the client, including HTTP headers. This function \
           registers exact path matches, i.e. `\"/users\"`, `\"/index.hml\"`, \
           etc. Use `harbor.%{Harbor.name}.register.regexp` to match regular \
           expressions. Paths are resolved in the order that they are declared \
           and can override default harbor paths such as metadata handlers. \
           The handler receives the request details as a record and a response \
           handler. The response handler can be used to fill up details about \
           the http response, which will be converted into a plain HTTP \
           response string after the handler returns. The request also \
           contains the low-level socket associated with the query which the \
           caller can use to implement their own custom response, if needed. \
           In this case, one should set `custom_response` to `true` on the \
           response handler."]
      (("", Lang.string_t, None, Some "path to serve.") :: register_args)
      Lang.unit_t
      (fun p ->
        let port, verb, handler = parse_register_args p in
        let uri = Lang.to_string (Lang.assoc "" 1 p) in
        let uri = Lang.Regexp.regexp [%string {|^%{uri}$|}] in
        Harbor.add_http_handler ~port ~verb ~uri handler;
        Lang.unit);

    Lang.add_builtin
      ("harbor." ^ Harbor.name ^ ".register.regexp")
      ~category:`Liquidsoap
      ~descr:
        [%string
          "Register a %{name_up} handler on the harbor. The handler function \
           receives as argument the full requested information and returns the \
           answer sent to the client, including HTTP headers. This function \
           registers regular expressions that are matched against the \
           request's path. The handler receives the request details as a \
           record and a response handler. The response handler can be used to \
           fill up details about the http response, which will be converted \
           into a plain HTTP response string after the handler returns. The \
           request also contains the low-level socket associated with the \
           query which the caller can use to implement their own custom \
           response, if needed. In this case, one should set `custom_response` \
           to `true` on the response handler."]
      (("", Lang.regexp_t, None, Some "path to to serve.") :: register_args)
      Lang.unit_t
      (fun p ->
        let port, verb, handler = parse_register_args p in
        let uri = Lang.to_regexp (Lang.assoc "" 1 p) in
        Harbor.add_http_handler ~port ~verb ~uri handler;
        Lang.unit)

  let () =
    Lang.add_builtin
      ("harbor." ^ Harbor.name ^ ".remove")
      ~category:`Liquidsoap
      ~descr:
        (Printf.sprintf "Remove a registered %s handler on the harbor." name_up)
      [
        ("port", Lang.int_t, Some (Lang.int 8000), Some "Port to server.");
        ( "method",
          Lang.string_t,
          Some (Lang.string "GET"),
          Some "Method served." );
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
end

module Unix_harbor = struct
  include Harbor

  let name = "http"
end

module Unix = Make (Unix_harbor)
include Unix
