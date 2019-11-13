(* -*- mode: tuareg; -*- *)
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

module type T = sig
  include Harbor.T

  val name : string
end

module Make (Harbor : T) = struct
  let name_up = String.uppercase_ascii Harbor.name

  let resp_t = Lang.string_getter_t ()

  let () =
    Lang_builtins.add_builtin
      ("harbor." ^ Harbor.name ^ ".register")
      ~cat:Liq
      ~descr:
        (Printf.sprintf
           "Register a %s handler on the harbor. The given function receives \
            as argument the full requested uri (e.g. \"foo?var=bar\"), http \
            protocol version, possible input data and the list of HTTP \
            headers and returns the answer sent to the client, including HTTP \
            headers. Registered uri can be regular expressions (e.g. \
            \".+\\.php\") and can override default metadata handlers. \
            Response is a string getter, i.e. either of type `string` or type \
            `()->string`. In the later case, getter function will be called \
            until it returns an empty string."
           name_up)
      [ ("port", Lang.int_t, None, Some "Port to server.");
        ("method", Lang.string_t, None, Some "Accepted method");
        ("", Lang.string_t, None, Some "URI to serve.");
        ( "",
          Lang.fun_t
            [ (false, "protocol", Lang.string_t);
              (false, "data", Lang.string_t);
              ( false,
                "headers",
                Lang.list_t (Lang.product_t Lang.string_t Lang.string_t) );
              (false, "", Lang.string_t) ]
            resp_t,
          None,
          Some
            "Function to execute. method argument is \"PUT\" or \"GET\", \
             protocol argument is \"HTTP/1.1\" or \"HTTP/1.0\" etc., data \
             argument contains data passed in case of a PUT request, and \"\" \
             otherwise. headers argument contains the HTTP headers. Unlabeled \
             argument contains the requested URI." ) ]
      Lang.unit_t
      (fun p ->
        let port = Lang.to_int (List.assoc "port" p) in
        let verb =
          Harbor.verb_of_string (Lang.to_string (List.assoc "method" p))
        in
        let uri = Lang.to_string (Lang.assoc "" 1 p) in
        let f = Lang.assoc "" 2 p in
        let f ~protocol ~data ~headers ~socket:_ uri =
          let l =
            List.map
              (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
              headers
          in
          let l =
            Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) l
          in
          let resp =
            Lang.apply ~t:resp_t f
              [ ("", Lang.string uri);
                ("headers", l);
                ("data", Lang.string data);
                ("protocol", Lang.string protocol) ]
          in
          try Harbor.simple_reply (Lang.to_string resp)
          with _ -> Harbor.reply (Lang.to_string_getter resp)
        in
        Harbor.add_http_handler ~port ~verb ~uri f ;
        Lang.unit)

  let () =
    Lang_builtins.add_builtin
      ("harbor." ^ Harbor.name ^ ".remove")
      ~cat:Liq
      ~descr:
        (Printf.sprintf "Remove a registered %s handler on the harbor." name_up)
      [ ("method", Lang.string_t, None, Some "Method served.");
        ("port", Lang.int_t, None, Some "Port to server.");
        ("", Lang.string_t, None, Some "URI served.") ]
      Lang.unit_t
      (fun p ->
        let port = Lang.to_int (List.assoc "port" p) in
        let uri = Lang.to_string (Lang.assoc "" 1 p) in
        let verb =
          Harbor.verb_of_string (Lang.to_string (List.assoc "method" p))
        in
        Harbor.remove_http_handler ~port ~verb ~uri () ;
        Lang.unit)
end

module Unix_harbor = struct
  include Harbor

  let name = "http"
end

module Unix = Make (Unix_harbor)
include Unix
