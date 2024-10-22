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

let decoder_metadata = Lang.add_module ~base:Modules.decoder "metadata"

let _ =
  let resolver_t =
    Lang.fun_t
      [(false, "metadata", Lang.metadata_t); (false, "", Lang.string_t)]
      Lang.metadata_t
  in
  Lang.add_builtin ~base:decoder_metadata "add" ~category:`Liquidsoap
    ~descr:"Register an external file metadata decoder."
    [
      ( "priority",
        Lang.getter_t Lang.int_t,
        Some (Lang.int 1),
        Some "Resolver's priority." );
      ( "mime_types",
        Lang.nullable_t (Lang.list_t Lang.string_t),
        Some Lang.null,
        Some
          "Decode files that match the mime types in this list. Accept any \
           file if `null`." );
      ( "file_extensions",
        Lang.nullable_t (Lang.list_t Lang.string_t),
        Some Lang.null,
        Some
          "Decode files that have the file extensions in this list. Accept any \
           file if `null`." );
      ("", Lang.string_t, None, Some "Format/resolver's name.");
      ( "",
        resolver_t,
        None,
        Some
          "Process to start. The function takes the format and filename as \
           argument and returns a list of (name,value) fields." );
    ]
    Lang.unit_t
    (fun p ->
      let format = Lang.to_string (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let mimes =
        Lang.to_valued_option
          (fun v -> List.map Lang.to_string (Lang.to_list v))
          (List.assoc "mime_types" p)
      in
      let extensions =
        Lang.to_valued_option
          (fun v -> List.map Lang.to_string (Lang.to_list v))
          (List.assoc "file_extensions" p)
      in
      let log = Log.make ["decoder"; "metadata"] in
      let priority = Lang.to_int_getter (List.assoc "priority" p) in
      let resolver ~metadata ~extension ~mime fname =
        if
          not (Decoder.test_file ~log ~extension ~mime ~mimes ~extensions fname)
        then raise Metadata.Invalid;
        let ret =
          Lang.apply f
            [("metadata", Lang.metadata metadata); ("", Lang.string fname)]
        in
        let ret = Lang.to_list ret in
        let ret = List.map Lang.to_product ret in
        let ret =
          List.map (fun (x, y) -> (Lang.to_string x, Lang.to_string y)) ret
        in
        ret
      in
      Plug.register Request.mresolvers format ~doc:""
        { Request.priority; resolver };
      Lang.unit)

let add_playlist_parser ~format name (parser : Playlist_parser.parser) =
  let return_t = Lang.list_t (Lang.product_t Lang.metadata_t Lang.string_t) in
  Lang.add_builtin ~base:Builtins_sys.playlist_parse name ~category:`Liquidsoap
    ~descr:(Printf.sprintf "Parse %s playlists" format)
    [
      ("", Lang.string_t, None, Some "Playlist file");
      ( "pwd",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Current directory to use for relative file path." );
    ]
    return_t
    (fun p ->
      let uri = Lang.to_string (List.assoc "" p) in
      let pwd = Lang.to_valued_option Lang.to_string (List.assoc "pwd" p) in
      let entries = parser ?pwd uri in
      Lang.list
        (List.map
           (fun (metadata, uri) ->
             Lang.product (Lang.metadata_list metadata) (Lang.string uri))
           entries))

let _ =
  let playlist_t = Lang.list_t (Lang.product_t Lang.metadata_t Lang.string_t) in
  let parser_t =
    Lang.fun_t
      [(true, "pwd", Lang.string_t); (false, "", Lang.string_t)]
      playlist_t
  in
  Lang.add_builtin ~base:Builtins_sys.playlist_parse "register"
    ~category:`Liquidsoap
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
    ]
    Lang.unit_t
    (fun p ->
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
        let ret = Lang.to_list (Lang.apply fn args) in
        if ret = [] then raise Not_found;
        List.map
          (fun el ->
            let m, s = Lang.to_product el in
            (Lang.to_metadata_list m, Lang.to_string s))
          ret
      in
      Plug.register Playlist_parser.parsers format ~doc:""
        { Playlist_parser.strict; Playlist_parser.parser = fn };
      Lang.unit)

let default_static =
  Lang.eval ~cache:false ~typecheck:false ~stdlib:`Disabled "fun (_) -> false"

let _ =
  let log_p = [("", "", None)] in
  let log_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t in
  let protocol_t =
    Lang.fun_t
      [
        (false, "rlog", log_t);
        (false, "maxtime", Lang.float_t);
        (false, "", Lang.string_t);
      ]
      Lang.(nullable_t string_t)
  in
  Lang.add_builtin ~base:Modules.protocol "add" ~category:`Liquidsoap
    ~descr:"Register a new protocol."
    [
      ( "temporary",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "if true, file is removed when it is finished." );
      ( "static",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.bool_t,
        Some default_static,
        Some
          "When given an uri for the protocol, if it returns `true`, then \
           requests can be resolved once and for all. Typically, static \
           protocols can be used to create infallible sources." );
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
      let static = List.assoc "static" p in
      let static s = Lang.to_bool (Lang.apply static [("", Lang.string s)]) in
      let doc = Lang.to_string (List.assoc "doc" p) in
      let syntax = Lang.to_string (List.assoc "syntax" p) in
      Lang.add_protocol ~syntax ~doc ~static name (fun arg ~log timeout ->
          let log =
            Lang.val_fun log_p (fun p ->
                let v = List.assoc "" p in
                log (Lang.to_string v);
                Lang.unit)
          in
          let ret =
            Lang.apply f
              [
                ("rlog", log);
                ("maxtime", Lang.float timeout);
                ("", Lang.string arg);
              ]
          in
          Option.map
            (fun s -> Request.indicator ~temporary (Lang.to_string s))
            (Lang.to_option ret));
      Lang.unit)

let _ =
  Lang.add_builtin ~base:Modules.protocol "count" ~category:`Liquidsoap
    ~descr:"Number of registered protocols." [] Lang.int_t (fun _ ->
      Doc.Protocol.count () |> Lang.int)
