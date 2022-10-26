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

module Value = Value.MkAbstract (struct
  type content = Request.t

  let name = "request"

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Requests cannot be represented as json"
      "json"

  let descr r = Printf.sprintf "<request(id=%d)>" (Request.get_id r)
  let compare = Stdlib.compare
end)

let () =
  Lang.add_builtin "request.create" ~category:`Liquidsoap
    ~descr:"Create a request from an URI."
    [
      ("indicators", Lang.list_t Lang.string_t, Some (Lang.list []), None);
      ( "persistent",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Indicate that the request is persistent, i.e. that it may be used \
           again once it has been played." );
      ( "temporary",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Indicate that the request is a temporary file: it will be destroyed \
           after being played." );
      ("", Lang.string_t, None, None);
    ]
    Value.t
    (fun p ->
      let indicators = List.assoc "indicators" p in
      let persistent = Lang.to_bool (List.assoc "persistent" p) in
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
      Value.to_value (Request.create ~persistent ~indicators initial))

let () =
  Lang.add_builtin "request.resolve" ~category:`Liquidsoap
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
      ("", Value.t, None, None);
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
        |> Option.map (fun s -> (Lang.to_source s)#ctype)
      in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let r = Value.of_value (List.assoc "" p) in
      Lang.bool
        (try Request.Resolved = Request.resolve ~ctype r timeout
         with _ -> false))

let () =
  Lang.add_builtin "request.read_metadata" ~category:`Liquidsoap
    [("", Value.t, None, None)]
    Lang.unit_t ~descr:"Force reading the metadata of a request."
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      Request.read_metadata r;
      Lang.unit)

let () =
  Lang.add_builtin "request.metadata" ~category:`Liquidsoap
    [("", Value.t, None, None)]
    Lang.metadata_t ~descr:"Get the metadata associated to a request."
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      Lang.metadata (Request.get_all_metadata r))

let () =
  Lang.add_builtin "request.log" ~category:`Liquidsoap
    [("", Value.t, None, None)]
    Lang.string_t ~descr:"Get log data associated to a request."
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      Lang.string (Request.string_of_log (Request.get_log r)))

let () =
  Lang.add_builtin "request.resolved" ~category:`Liquidsoap
    ~descr:
      "Check if a request is resolved, i.e. is associated to a valid local \
       file."
    [("", Value.t, None, None)]
    Lang.bool_t
    (fun p ->
      let e = Value.of_value (List.assoc "" p) in
      Lang.bool (Request.resolved e))

let () =
  Lang.add_builtin "request.uri" ~category:`Liquidsoap
    ~descr:"Initial URI of a request."
    [("", Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      Lang.string (Request.initial_uri r))

let () =
  Lang.add_builtin "request.filename" ~category:`Liquidsoap
    ~descr:
      "Return a valid local filename if the request is ready, and the empty \
       string otherwise."
    [("", Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      Lang.string (match Request.get_filename r with Some f -> f | None -> ""))

let () =
  Lang.add_builtin "request.destroy" ~category:`Liquidsoap
    ~descr:
      "Destroying a request causes any temporary associated file to be \
       deleted, and releases its RID. Persistent requests resist to \
       destroying, unless forced."
    [
      ( "force",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Destroy the request even if it is persistent." );
      ("", Value.t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let force = Lang.to_bool (List.assoc "force" p) in
      let e = Value.of_value (List.assoc "" p) in
      Request.destroy ~force e;
      Lang.unit)

let () =
  Lang.add_builtin "request.duration" ~category:`Liquidsoap
    [("", Lang.string_t, None, None)]
    Lang.float_t
    ~descr:
      "Compute the duration in seconds of audio data contained in a request. \
       The computation may be expensive. Returns -1. if computation failed, \
       typically if the file was not recognized as valid audio."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.float (try Request.duration f with Not_found -> -1.))

let () =
  Lang.add_builtin "request.id" ~category:`Liquidsoap
    ~descr:"Identifier of a request."
    [("", Value.t, None, None)]
    Lang.int_t
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      Lang.int (Request.get_id r))

let () =
  Lang.add_builtin "request.status" ~category:`Liquidsoap
    ~descr:
      "Current status of a request. Can be idle, resolving, ready, playing or \
       destroyed."
    [("", Value.t, None, None)]
    Lang.string_t
    (fun p ->
      let r = Value.of_value (List.assoc "" p) in
      let s =
        match Request.status r with
          | Request.Idle -> "idle"
          | Request.Resolving -> "resolving"
          | Request.Ready -> "ready"
          | Request.Playing -> "playing"
          | Request.Destroyed -> "destroyed"
      in
      Lang.string s)
