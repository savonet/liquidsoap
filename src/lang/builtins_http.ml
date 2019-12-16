(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type request = Get | Post | Put | Head | Delete

let add_http_request http name descr request =
  let log = Log.make [name] in
  let header_t = Lang.product_t Lang.string_t Lang.string_t in
  let headers_t = Lang.list_t header_t in
  let status_t = Lang.tuple_t [Lang.string_t; Lang.int_t; Lang.string_t] in
  let request_return_t = Lang.tuple_t [status_t; headers_t; Lang.string_t] in
  let params =
    if List.mem request [Get; Head; Delete] then []
    else [("data", Lang.string_t, Some (Lang.string ""), Some "POST data.")]
  in
  let params =
    params
    @ [
        ( "headers",
          headers_t,
          Some (Lang.list ~t:header_t []),
          Some "Additional headers." );
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
      let url = Lang.to_string (List.assoc "" p) in
      let (x, y, z), headers, data =
        try
          let uri = Http.parse_url url in
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
          Http.full_request ~log ~timeout ~headers ~uri ~request ()
        with e ->
          (* Here we return a fake code.. *)
          ( ("Internal error", 999, "Internal error"),
            [],
            Printf.sprintf "Error while processing request: %s"
              (Printexc.to_string e) )
      in
      let status = Lang.tuple [Lang.string x; Lang.int y; Lang.string z] in
      let headers =
        List.map
          (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
          headers
      in
      let headers = Lang.list ~t:header_t headers in
      Lang.tuple [status; headers; Lang.string data])

let () =
  let add_http_request = add_http_request (module Http) in
  add_http_request "http.get"
    "Perform a full Http GET request and return `(status,headers,data)`." Get;
  add_http_request "http.post"
    "Perform a full Http POST request and return `(status,headers,data)`." Post;
  add_http_request "http.put"
    "Perform a full Http PUT request and return `(status,headers,data)`." Put;
  add_http_request "http.head"
    "Perform a full Http HEAD request and return `(status,headers,data)`." Head;
  add_http_request "http.delete"
    "Perform a full Http DELETE request and return `(status,headers,data)`."
    Delete
