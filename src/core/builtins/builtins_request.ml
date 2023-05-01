(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  Lang.add_builtin ~base:request "create" ~category:`Liquidsoap
    ~descr:"Create a request from an URI."
    [
      ("indicators", Lang.list_t Lang.string_t, Some (Lang.list []), None);
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
      let indicators = List.assoc "indicators" p in
      let persistent = Lang.to_bool (List.assoc "persistent" p) in
      let resolve_metadata = Lang.to_bool (List.assoc "resolve_metadata" p) in
      let initial = Lang.to_string (List.assoc "" p) in
      let l = String.length initial in
      let initial =
        (* Remove trailing newline *)
        if l > 0 && initial.[l - 1] = '\n' then String.sub initial 0 (l - 1)
        else initial
      in
      let indicators = List.map Lang.to_string (Lang.to_list indicators) in
      let indicators = List.map (fun x -> Request.indicator x) indicators in
      let temporary = List.assoc "temporary" p |> Lang.to_bool in
      let indicators =
        if temporary then
          Request.indicator ~temporary:true initial :: indicators
        else indicators
      in
      Request.Value.to_value
        (Request.create ~resolve_metadata ~persistent ~indicators initial))

let _ =
  Lang.add_builtin ~base:request "resolve" ~category:`Liquidsoap
    [
      ( "content_type",
        Lang.nullable_t (Lang.source_t (Lang.univ_t ())),
        Some Lang.null,
        Some
          "If specified, the request will be decoded with the same content \
           type as the given source." );
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
      let ctype =
        List.assoc "content_type" p
        |> Lang.to_option
        |> Option.map (fun s -> (Lang.to_source s)#content_type)
      in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.bool
        (try Request.Resolved = Request.resolve ~ctype r timeout
         with _ -> false))

let _ =
  Lang.add_builtin ~base:request "read_metadata" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.unit_t ~descr:"Force reading the metadata of a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Request.read_metadata r;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:request "metadata" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.metadata_t ~descr:"Get the metadata associated to a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.metadata (Request.get_all_metadata r))

let _ =
  Lang.add_builtin ~base:request "log" ~category:`Liquidsoap
    [("", Request.Value.t, None, None)]
    Lang.string_t ~descr:"Get log data associated to a request."
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.string (Request.string_of_log (Request.get_log r)))

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
  Lang.add_builtin ~base:request "duration" ~category:`Liquidsoap
    [("", Lang.string_t, None, None)]
    (Lang.nullable_t Lang.float_t)
    ~descr:
      "Compute the duration in seconds of audio data contained in a request. \
       The computation may be expensive. Returns `null` if computation failed, \
       typically if the file was not recognized as valid audio."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      try Lang.float (Request.duration f) with _ -> Lang.null)

let _ =
  Lang.add_builtin ~base:request "id" ~category:`Liquidsoap
    ~descr:"Identifier of a request."
    [("", Request.Value.t, None, None)]
    Lang.int_t
    (fun p ->
      let r = Request.Value.of_value (List.assoc "" p) in
      Lang.int (Request.get_id r))

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
          | Request.Idle -> "idle"
          | Request.Resolving -> "resolving"
          | Request.Ready -> "ready"
          | Request.Playing -> "playing"
          | Request.Destroyed -> "destroyed"
      in
      Lang.string s)
