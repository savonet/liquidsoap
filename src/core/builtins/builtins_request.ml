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

let request = Modules.request

let _ =
  Lang.add_builtin ~base:request "all" ~category:`Liquidsoap
    ~descr:"Return all the requests currently available." []
    (Lang.list_t Request.Value.t) (fun _ ->
      Lang.list (List.map Request.Value.to_value (Request.all ())))

let _ =
  Lang.add_builtin ~base:request "is_static" ~category:`Liquidsoap
    ~descr:"`true` if the given URI is assumed to be static, e.g. a file."
    [("", Lang.string_t, None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Request.is_static (Lang.to_string (List.assoc "" p))))

let _ =
  Lang.add_builtin ~base:request "create" ~category:`Liquidsoap
    ~descr:"Create a request from an URI."
    [
      ( "cue_in_metadata",
        Lang.nullable_t Lang.string_t,
        Some (Lang.string "liq_cue_in"),
        Some "Metadata for cue in points. Disabled if `null`." );
      ( "cue_out_metadata",
        Lang.nullable_t Lang.string_t,
        Some (Lang.string "liq_cue_out"),
        Some "Metadata for cue out points. Disabled if `null`." );
      ( "persistent",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Indicate that the request is persistent, i.e. that it may be used \
           again once it has been played." );
      ( "resolve_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Set to `false` to prevent metadata resolution on this request." );
      ( "excluded_metadata_resolvers",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some "List of metadata resolves to exclude when resolving metadata." );
      ( "temporary",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Indicate that the request is a temporary file: it will be destroyed \
           after being played." );
      ("", Lang.string_t, None, None);
    ]
    Request.Value.t
    (fun p ->
      let persistent = Lang.to_bool (List.assoc "persistent" p) in
      let resolve_metadata = Lang.to_bool (List.assoc "resolve_metadata" p) in
      let excluded_metadata_resolvers =
        List.map Lang.to_string
          (Lang.to_list (List.assoc "excluded_metadata_resolvers" p))
      in
      let cue_in_metadata =
        Lang.to_valued_option Lang.to_string (List.assoc "cue_in_metadata" p)
      in
      let cue_out_metadata =
        Lang.to_valued_option Lang.to_string (List.assoc "cue_out_metadata" p)
      in
      let initial = Lang.to_string (List.assoc "" p) in
      let l = String.length initial in
      let initial =
        (* Remove trailing newline *)
        if l > 0 && initial.[l - 1] = '\n' then String.sub initial 0 (l - 1)
        else initial
      in
      let temporary = List.assoc "temporary" p |> Lang.to_bool in
      Request.Value.to_value
        (Request.create ~resolve_metadata ~persistent
           ~excluded_metadata_resolvers ~cue_in_metadata ~cue_out_metadata
           ~temporary initial))

let _ =
  Lang.add_builtin ~base:request "resolve" ~category:`Liquidsoap
    [
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 30.),
        Some "Limit in seconds to the duration of the resolving." );
      ("", Request.Value.t, None, None);
    ]
    Lang.bool_t
    ~descr:
      "Resolve a request, i.e. attempt to get a valid local file. The \
       operation can take some time. Return true if the resolving was \
       successful, false otherwise (timeout or invalid URI). The request \
       should not be decoded afterward: this is mostly useful to download \
       files such as playlists, etc."
    (fun p ->
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.bool (try Request.resolve r timeout = `Resolved with _ -> false))

let _ =
  Lang.add_builtin ~base:request "metadata" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.metadata_t ~descr:"Get the metadata associated to a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.metadata (Request.metadata r))

let _ =
  Lang.add_builtin ~base:request "log" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.string_t ~descr:"Get log data associated to a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (Request.log r))

let _ =
  Lang.add_builtin ~base:request "resolved" ~category:`Liquidsoap
    ~descr:
      "Check if a request is resolved, i.e. is associated to a valid local \
       file."
    [("", Request.Value.t, None, None)]
    Lang.bool_t
    (fun p ->
      let e = Request.Value.of_value (List.assoc "" p) in
      Lang.bool (Request.resolved e))

let _ =
  Lang.add_builtin ~base:request "uri" ~category:`Liquidsoap
    ~descr:"Initial URI of a request."
    [("", Request.Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (Request.initial_uri r))

let _ =
  Lang.add_builtin ~base:request "filename" ~category:`Liquidsoap
    ~descr:
      "Return a valid local filename if the request is ready, and the empty \
       string otherwise."
    [("", Request.Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (match Request.get_filename r with Some f -> f | None -> ""))

let _ =
  Lang.add_builtin ~base:request "destroy" ~category:`Liquidsoap
    ~descr:
      "Destroying a request causes any temporary associated file to be \
       deleted, and releases its RID. Persistent requests resist to \
       destroying, unless forced."
    [
      ( "force",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Destroy the request even if it is persistent." );
      ("", Request.Value.t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let force = Lang.to_bool (List.assoc "force" p) in
      let e = Request.Value.of_value (List.assoc "" p) in
      Request.destroy ~force e;
      Lang.unit)

let _ =
  let add_duration_resolver ~base ~name ~resolver () =
    Lang.add_builtin ~base name ~category:`Liquidsoap
      ((if resolver = None then
          [
            ( "resolvers",
              Lang.nullable_t (Lang.list_t Lang.string_t),
              Some Lang.null,
              Some
                "Set to a list of resolvers to only resolve duration using a \
                 specific decoder." );
          ]
        else [])
      @ [
          ( "resolve_metadata",
            Lang.bool_t,
            Some (Lang.bool true),
            Some
              "Set to `false` to prevent metadata resolution on this request."
          );
          ( "metadata",
            Lang.metadata_t,
            Some (Lang.list []),
            Some
              "Optional metadata used to decode the file, e.g. \
               `ffmpeg_options`." );
          ( "timeout",
            Lang.float_t,
            Some (Lang.float 30.),
            Some "Limit in seconds to the duration of the resolving." );
          ("", Lang.string_t, None, None);
        ])
      (Lang.nullable_t Lang.float_t)
      ~descr:
        (Printf.sprintf
           "Compute the duration in seconds of audio data contained in a \
            request%s. The computation may be expensive. Returns `null` if \
            computation failed, typically if the file was not recognized as \
            valid audio."
           (match resolver with
             | Some r -> " using the " ^ r ^ " decoder"
             | None -> ""))
      (fun p ->
        let f = Lang.to_string (List.assoc "" p) in
        let resolve_metadata = Lang.to_bool (List.assoc "resolve_metadata" p) in
        let resolvers =
          match resolver with
            | None ->
                Option.map (List.map Lang.to_string)
                  (Lang.to_valued_option Lang.to_list (List.assoc "resolvers" p))
            | Some r -> Some [r]
        in
        let metadata = Lang.to_metadata (List.assoc "metadata" p) in
        let timeout = Lang.to_float (List.assoc "timeout" p) in
        let r =
          Request.create ~resolve_metadata ~metadata ~cue_in_metadata:None
            ~cue_out_metadata:None f
        in
        if Request.resolve r timeout = `Resolved then (
          match
            Request.duration ?resolvers ~metadata:(Request.metadata r)
              (Option.get (Request.get_filename r))
          with
            | Some f -> Lang.float f
            | None -> Lang.null
            | exception exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"failure" exn)
        else Lang.null)
  in
  let base =
    add_duration_resolver ~base:request ~name:"duration" ~resolver:None ()
  in
  List.iter
    (fun name ->
      ignore (add_duration_resolver ~base ~name ~resolver:(Some name) ()))
    Request.conf_dresolvers#get

let _ =
  Lang.add_builtin ~base:request "id" ~category:`Liquidsoap
    ~descr:"Identifier of a request."
    [("", Request.Value.t, None, None)]
    Lang.int_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.int (Request.id r))

let _ =
  Lang.add_builtin ~base:request "status" ~category:`Liquidsoap
    ~descr:
      "Current status of a request. Can be idle, resolving, ready, playing or \
       destroyed."
    [("", Request.Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      let s =
        match Request.status r with
          | `Idle -> "idle"
          | `Resolving _ -> "resolving"
          | `Ready -> "ready"
          | `Destroyed -> "destroyed"
          | `Failed -> "failed"
      in
      Lang.string s)
