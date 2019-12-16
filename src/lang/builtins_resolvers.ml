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

let () =
  let resolver_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.metadata_t in
  add_builtin "add_metadata_resolver" ~cat:Liq
    ~descr:"Register an external file metadata decoder."
    [
      ("", Lang.string_t, None, Some "Format/resolver's name.");
      ( "",
        resolver_t,
        None,
        Some
          "Process to start. The function takes the format and filename as \
           argument and returns a list of (name,value) fields." );
    ] Lang.unit_t (fun p ->
      let format = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let resolver name =
        let ret = Lang.apply ~t:Lang.metadata_t f [("", Lang.string name)] in
        let ret = Lang.to_list ret in
        let ret = List.map Lang.to_product ret in
        let ret =
          List.map (fun (x, y) -> (Lang.to_string x, Lang.to_string y)) ret
        in
        ret
      in
      Request.mresolvers#register format resolver;
      Lang.unit)

let () =
  let playlist_t = Lang.list_t (Lang.product_t Lang.metadata_t Lang.string_t) in
  let parser_t =
    Lang.fun_t
      [(true, "pwd", Lang.string_t); (false, "", Lang.string_t)]
      playlist_t
  in
  add_builtin "add_playlist_parser" ~cat:Liq
    ~descr:
      "Register a new playlist parser. An empty playlist is considered as a \
       failure to resolve."
    [
      ( "format",
        Lang.string_t,
        None,
        Some "Playlist format. If possible, a mime-type." );
      ( "strict",
        Lang.bool_t,
        None,
        Some "True if playlist format can be detected unambiguously." );
      ("", parser_t, None, Some "Playlist parser");
    ] Lang.unit_t (fun p ->
      let format = Lang.to_string (List.assoc "format" p) in
      let strict = Lang.to_bool (List.assoc "strict" p) in
      let fn = List.assoc "" p in
      let fn ?pwd uri =
        let args = [("", Lang.string uri)] in
        let args =
          match pwd with
            | Some pwd -> ("pwd", Lang.string pwd) :: args
            | None -> args
        in
        let ret = Lang.to_list (Lang.apply ~t:playlist_t fn args) in
        if ret = [] then raise Not_found;
        List.map
          (fun el ->
            let m, s = Lang.to_product el in
            (Lang.to_metadata_list m, Lang.to_string s))
          ret
      in
      Playlist_parser.parsers#register format
        { Playlist_parser.strict; Playlist_parser.parser = fn };
      Lang.unit)

let () =
  let log_p = [("", "", Lang.string_t, None)] in
  let log_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t in
  let protocol_t =
    Lang.fun_t
      [
        (false, "rlog", log_t);
        (false, "maxtime", Lang.float_t);
        (false, "", Lang.string_t);
      ]
      (Lang.list_t Lang.string_t)
  in
  add_builtin "add_protocol" ~cat:Liq ~descr:"Register a new protocol."
    [
      ( "temporary",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "if true, file is removed when it is finished." );
      ( "static",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "if true, then requests can be resolved once and for all. Typically, \
           static protocols can be used to create infallible sources." );
      ( "syntax",
        Lang.string_t,
        Some (Lang.string "Undocumented"),
        Some "URI syntax." );
      ( "doc",
        Lang.string_t,
        Some (Lang.string "Undocumented"),
        Some "Protocol documentation." );
      ( "",
        Lang.string_t,
        None,
        Some
          "Protocol name. Resolver will be called on uris of the form: \
           `<protocol name>:...`." );
      ( "",
        protocol_t,
        None,
        Some
          "Protocol resolver. Receives a function to log protocol resolution, \
           the `<arg>` in `<protocol name>:<arg>` and the max delay that \
           resolution should take." );
    ]
    Lang.unit_t
    (fun p ->
      let name = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let temporary = Lang.to_bool (List.assoc "temporary" p) in
      let static = Lang.to_bool (List.assoc "static" p) in
      let doc = Lang.to_string (List.assoc "doc" p) in
      let syntax = Lang.to_string (List.assoc "syntax" p) in
      Lang.add_protocol ~syntax ~doc ~static name (fun arg ~log timeout ->
          let log =
            Lang.val_fun log_p ~ret_t:Lang.unit_t (fun p _ ->
                let v = List.assoc "" p in
                log (Lang.to_string v);
                Lang.unit)
          in
          let l =
            Lang.apply
              ~t:(Lang.list_t Lang.string_t)
              f
              [
                ("rlog", log);
                ("maxtime", Lang.float timeout);
                ("", Lang.string arg);
              ]
          in
          List.map
            (fun s -> Request.indicator ~temporary (Lang.to_string s))
            (Lang.to_list l));
      Lang.unit)
