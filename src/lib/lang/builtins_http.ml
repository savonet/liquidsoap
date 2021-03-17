(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

open Lang_builtins

let () =
  Lang.add_module "http";
  Lang.add_module "https";
  Lang.add_module "harbor"

type request = Get | Post | Put | Head | Delete

let add_http_error kind =
  Lang.add_builtin_base ~category:(string_of_category Liq)
    ~descr:(Printf.sprintf "Base error for %s" kind)
    (Printf.sprintf "%s.error" kind)
    (Builtins_error.Error.to_value { Builtins_error.kind; msg = None })
      .Lang.value Builtins_error.Error.t

let () =
  add_http_error "http";
  add_http_error "https"

let add_http_request http m name descr request =
  let name = Printf.sprintf "%s.%s" m name in
  let log = Log.make [name] in
  let header_t = Lang.product_t Lang.string_t Lang.string_t in
  let headers_t = Lang.list_t header_t in
  let request_return_t =
    Lang.method_t Lang.string_t
      [
        ( "protocol_version",
          ([], Lang.string_t),
          "Version of the HTTP protocol." );
        ("status_code", ([], Lang.int_t), "Status code.");
        ("status_message", ([], Lang.string_t), "Status message.");
        ("headers", ([], headers_t), "HTTP headers.");
      ]
  in
  let params =
    if List.mem request [Get; Head; Delete] then []
    else [("data", Lang.string_t, Some (Lang.string ""), Some "POST data.")]
  in
  let params =
    params
    @ [
        ("headers", headers_t, Some (Lang.list []), Some "Additional headers.");
        ( "http_version",
          Lang.string_t,
          Some (Lang.string "1.0"),
          Some "Http request version." );
        ( "redirect",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Perform redirections if needed." );
        ( "timeout",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Timeout for network operations." );
        ( "",
          Lang.string_t,
          None,
          Some "Requested URL, e.g. \"http://www.google.com:80/index.html\"." );
      ]
  in
  let (module Http : Http.Http_t) = http in
  add_builtin name ~cat:Interaction ~descr params request_return_t (fun p ->
      let headers = List.assoc "headers" p in
      let headers = Lang.to_list headers in
      let headers = List.map Lang.to_product headers in
      let headers =
        List.map (fun (x, y) -> (Lang.to_string x, Lang.to_string y)) headers
      in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let http_version = Lang.to_string (List.assoc "http_version" p) in
      let url = Lang.to_string (List.assoc "" p) in
      let redirect = Lang.to_bool (List.assoc "redirect" p) in
      let (protocol_version, status_code, status_message), headers, data =
        try
          let request =
            match request with
              | Get -> Http.Get
              | Post ->
                  let data = Lang.to_string (List.assoc "data" p) in
                  Http.Post data
              | Put ->
                  let data = Lang.to_string (List.assoc "data" p) in
                  Http.Put data
              | Head -> Http.Head
              | Delete -> Http.Delete
          in
          let log s = log#info "%s" s in
          let rec f url =
            let uri = Http.parse_url url in
            let ans =
              Http.full_request ~log ~timeout ~headers ~uri ~request
                ~http_version ()
            in
            let (_, status_code, _), headers, _ = ans in
            if
              redirect
              && (status_code = 301 || status_code = 307)
              && List.mem_assoc "location" headers
            then f (List.assoc "location" headers)
            else ans
          in
          f url
        with e ->
          raise
            (Lang_values.Runtime_error
               {
                 Lang_values.kind = m;
                 msg = Some (Printexc.to_string e);
                 pos = [];
               })
      in
      let protocol_version = Lang.string protocol_version in
      let status_code = Lang.int status_code in
      let status_message = Lang.string status_message in
      let headers =
        List.map
          (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
          headers
      in
      let headers = Lang.list headers in
      Lang.meth (Lang.string data)
        [
          ("protocol_version", protocol_version);
          ("status_code", status_code);
          ("status_message", status_message);
          ("headers", headers);
        ])

let () =
  let add_http_request = add_http_request (module Http) in
  add_http_request "http" "get" "Perform a full Http GET request." Get;
  add_http_request "http" "post" "Perform a full Http POST request`." Post;
  add_http_request "http" "put" "Perform a full Http PUT request." Put;
  add_http_request "http" "head" "Perform a full Http HEAD request." Head;
  add_http_request "http" "delete" "Perform a full Http DELETE request." Delete
