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

type request = Get | Post | Put | Head | Delete

module Http = Liq_http

let conf_http =
  Dtools.Conf.void ~p:(Configure.conf#plug "http") "Settings for HTTP requests"

let conf_normalize_url =
  Dtools.Conf.bool ~d:true
    ~p:(conf_http#plug "normalize_url")
    "When `true`, HTTP urls are normalized by default, i.e. spaces are \
     replaced with `%20` and etc."

let string_of_request = function
  | Get -> "get"
  | Post -> "post"
  | Put -> "put"
  | Head -> "head"
  | Delete -> "delete"

let request_with_body = [Get; Post; Put]
let http = Modules.http
let http_transport = Modules.http_transport

let _ =
  Lang.add_builtin_value ~category:`Internet ~descr:"Http unencrypted transport"
    ~base:http_transport "unix"
    (Lang.http_transport Http.unix_transport)
    Lang.http_transport_t

let add_http_request ~base ~stream_body ~descr ~request name =
  let header_t = Lang.product_t Lang.string_t Lang.string_t in
  let headers_t = Lang.list_t header_t in
  let has_body = List.mem request request_with_body in
  let log = Log.make ["http"; string_of_request request] in
  let request_return_t =
    Lang.method_t
      (if (not has_body) || stream_body then Lang.unit_t else Lang.string_t)
      [
        ("http_version", ([], Lang.string_t), "Version of the HTTP protocol.");
        ("status_code", ([], Lang.int_t), "Status code.");
        ("status_message", ([], Lang.string_t), "Status message.");
        ("headers", ([], headers_t), "HTTP headers.");
      ]
  in
  let params =
    if List.mem request [Get; Head; Delete] then []
    else
      [
        ( "data",
          Lang.getter_t Lang.string_t,
          Some (Lang.string ""),
          Some
            "POST data. Use a `string` getter to stream data and return `\"\"` \
             when all data has been passed." );
      ]
  in
  let params =
    params
    @ [
        ("headers", headers_t, Some (Lang.list []), Some "Additional headers.");
        ( "http_version",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some "Http request version." );
        ( "redirect",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Perform redirections if needed." );
        ( "timeout",
          Lang.nullable_t Lang.float_t,
          Some (Lang.float 10.),
          Some "Timeout for network operations in seconds." );
        ( "normalize_url",
          Lang.nullable_t Lang.bool_t,
          Some Lang.null,
          Some
            "Normalize url, replacing spaces with `%20` and more. Defaults to \
             `settings.http.normalize_url` when `null`." );
        ( "",
          Lang.string_t,
          None,
          Some "Requested URL, e.g. `\"http://www.liquidsoap.info/\"`." );
      ]
    @
    if stream_body then
      [
        ( "on_body_data",
          Lang.fun_t [(false, "", Lang.nullable_t Lang.string_t)] Lang.unit_t,
          None,
          Some
            "function called when receiving response body data. `null` or \
             `\"\"` means that all the data has been passed." );
      ]
    else []
  in
  Lang.add_builtin name ~base ~category:`Internet ~descr params request_return_t
    (fun p ->
      let headers = List.assoc "headers" p in
      let headers = Lang.to_list headers in
      let headers = List.map Lang.to_product headers in
      let headers =
        List.map (fun (x, y) -> (Lang.to_string x, Lang.to_string y)) headers
      in
      let timeout =
        Lang.to_valued_option
          (fun v -> int_of_float (1000. *. Lang.to_float v))
          (List.assoc "timeout" p)
      in
      let http_version =
        Option.map Lang.to_string (Lang.to_option (List.assoc "http_version" p))
      in
      let original_url = Lang.to_string (List.assoc "" p) in
      let normalize_url =
        Option.value ~default:conf_normalize_url#get
          (Lang.to_valued_option Lang.to_bool (List.assoc "normalize_url" p))
      in
      let url =
        if normalize_url then Uri.(to_string (of_string original_url))
        else original_url
      in
      if Uri.pct_decode url <> original_url then
        log#important
          "Requested url %s is different from normalized url: %s. URL are \
           normalized by default to ensure maximum compatibility with e.g. \
           URLs with spaces in them. However, this can also cause issues so we \
           recommend passing normalized URLs. Url normalization can be \
           disabled on a case-by-case basis using the `normalize_url` \
           parameter or globally using the `settings.http.normalize_url` \
           setting."
          (Lang_string.quote_utf8_string original_url)
          (Lang_string.quote_utf8_string (Uri.pct_decode url));
      let redirect = Lang.to_bool (List.assoc "redirect" p) in
      let on_body_data, get_body =
        if stream_body then (
          let on_body_data = List.assoc "on_body_data" p in
          ( (fun s ->
              let v =
                match s with None -> Lang.null | Some s -> Lang.string s
              in
              ignore (Lang.apply on_body_data [("", v)])),
            fun _ -> assert false ))
        else (
          let body = Buffer.create 1024 in
          ( (fun s -> ignore (Option.map (Buffer.add_string body) s)),
            fun () -> Buffer.contents body ))
      in
      let get_data () =
        let data = List.assoc "data" p in
        let buf = Buffer.create 10 in
        let len, refill =
          try
            let data = Lang.to_string data in
            Buffer.add_string buf data;
            (Some (Int64.of_int (String.length data)), fun () -> ())
          with _ ->
            let fn = Lang.to_getter data in
            (None, fun () -> Buffer.add_string buf (Lang.to_string (fn ())))
        in
        ( len,
          fun len ->
            if Buffer.length buf = 0 then refill ();
            let len = min (Buffer.length buf) len in
            let ret = Buffer.sub buf 0 len in
            Utils.buffer_drop buf len;
            ret )
      in
      let http_version, status_code, status_message, headers =
        try
          let request =
            match request with
              | Get -> `Get
              | Post -> `Post (get_data ())
              | Put -> `Put (get_data ())
              | Head -> `Head
              | Delete -> `Delete
          in
          let ans =
            Liqcurl.http_request ~pos:(Lang.pos p) ~follow_redirect:redirect
              ~timeout ~headers ~url
              ~on_body_data:(fun s -> on_body_data (Some s))
              ~request ?http_version ()
          in
          on_body_data None;
          ans
        with
          | Curl.CurlException (Curl.CURLE_ABORTED_BY_CALLBACK, _, _) ->
              ("1.0", 520, "Operation aborted", [])
          | Curl.(CurlException (CURLE_OPERATION_TIMEOUTED, _, _)) ->
              ("1.0", 522, "Connection timed out", [])
          | Curl.(CurlException (CURLE_COULDNT_CONNECT, _, _))
          | Curl.(CurlException (CURLE_COULDNT_RESOLVE_HOST, _, _)) ->
              ("1.0", 523, "Origin is unreachable", [])
          | Curl.(CurlException (CURLE_GOT_NOTHING, _, _)) ->
              ("1.0", 523, "Remote server did not return any data", [])
          | Curl.(CurlException (CURLE_SSL_CONNECT_ERROR, _, _)) ->
              ("1.0", 525, "SSL handshake failed", [])
          | Curl.(CurlException (CURLE_SSL_CACERT, _, _)) ->
              ("1.0", 526, "Invalid SSL certificate", [])
          | e ->
              let bt = Printexc.get_raw_backtrace () in
              Lang.log#severe "Could not perform http request: %s."
                (Printexc.to_string e);
              Lang.raise_as_runtime ~bt ~kind:"http" e
      in
      let http_version = Lang.string http_version in
      let status_code = Lang.int status_code in
      let status_message = Lang.string status_message in
      let headers =
        List.map
          (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
          headers
      in
      let headers = Lang.list headers in
      let ret =
        if (not has_body) || stream_body then Lang.unit
        else Lang.string (get_body ())
      in
      Lang.meth ret
        [
          ("http_version", http_version);
          ("status_code", status_code);
          ("status_message", status_message);
          ("headers", headers);
        ])

let () =
  List.iter
    (fun (request, name) ->
      let base =
        add_http_request ~base:http
          ~descr:
            ("Perform a full http " ^ String.uppercase_ascii name ^ " request.")
          ~request ~stream_body:false name
      in
      ignore
        (add_http_request ~base
           ~descr:
             ("Perform a full http " ^ String.uppercase_ascii name ^ " request.")
           ~request ~stream_body:true "stream"))
    [
      (Get, "get");
      (Post, "post");
      (Put, "put");
      (Head, "head");
      (Delete, "delete");
    ]

let _ =
  Lang.add_builtin_base ~category:`Internet ~descr:"Default user-agent"
    ~base:http "user_agent" (`String Http.user_agent) Lang.string_t
