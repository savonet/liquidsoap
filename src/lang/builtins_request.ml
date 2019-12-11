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
  add_builtin "request.create.raw" ~cat:Liq
    ~descr:
      "Create a raw request, for files that should not be decoded for \
       streaming such as playlists. Creation may fail if there is no \
       available RID, which cannot be detected currently: in that case one \
       will obtain a request that will fail to be resolved."
    [ ( "indicators",
        Lang.list_t Lang.string_t,
        Some (Lang.list ~t:Lang.string_t []),
        None );
      ("persistent", Lang.bool_t, Some (Lang.bool false), None);
      ("", Lang.string_t, None, None) ]
    (Lang.request_t
       (Lang.frame_kind_t ~audio:Lang.zero_t ~video:Lang.zero_t
          ~midi:Lang.zero_t))
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
      Lang.request (Request.create_raw ~persistent ~indicators initial))

let () =
  Lang.add_builtin "request.create" ~category:(string_of_category Liq)
    ~descr:
      "Create a request. Creation may fail if there is no available RID, \
       which cannot be detected currently: in that case one will obtain a \
       request that will fail to be resolved."
    [ ( "indicators",
        Lang.list_t Lang.string_t,
        Some (Lang.list ~t:Lang.string_t []),
        None );
      ("persistent", Lang.bool_t, Some (Lang.bool false), None);
      ("", Lang.string_t, None, None) ]
    (Lang.request_t (Lang.univ_t ()))
    (fun p t ->
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
      let kind =
        let k_t = Lang.of_request_t t in
        Lang.frame_kind_of_kind_type k_t
      in
      Lang.request (Request.create ~kind ~persistent ~indicators initial))

let () =
  add_builtin "request.resolve" ~cat:Liq
    [ ( "timeout",
        Lang.float_t,
        Some (Lang.float 30.),
        Some "Limit in seconds to the duration of the resolving." );
      ("", Lang.request_t (Lang.univ_t ()), None, None) ]
    Lang.bool_t
    ~descr:
      "Resolve a request, i.e. attempt to get a valid local file. The \
       operation can take some time. Return true if the resolving was \
       successful, false otherwise (timeout or invalid URI)."
    (fun p ->
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let r = Lang.to_request (List.assoc "" p) in
      Lang.bool
        (try Request.Resolved = Request.resolve r timeout with _ -> false))

let () =
  add_builtin "request.metadata" ~cat:Liq
    [("", Lang.request_t (Lang.univ_t ()), None, None)]
    Lang.metadata_t ~descr:"Get the metadata associated to a request."
    (fun p ->
      let r = Lang.to_request (List.assoc "" p) in
      Lang.metadata (Request.get_all_metadata r))

let () =
  add_builtin "request.log" ~cat:Liq
    [("", Lang.request_t (Lang.univ_t ()), None, None)]
    Lang.string_t ~descr:"Get log data associated to a request."
    (fun p ->
      let r = Lang.to_request (List.assoc "" p) in
      Lang.string (Request.string_of_log (Request.get_log r)))

let () =
  add_builtin "request.ready" ~cat:Liq
    ~descr:
      "Check if a request is ready, i.e. is associated to a valid local file. \
       Unless the initial URI was such a file, a request has to be resolved \
       before being ready."
    [("", Lang.request_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p ->
      let e = Lang.to_request (List.assoc "" p) in
      Lang.bool (Request.is_ready e))

let () =
  add_builtin "request.uri" ~cat:Liq ~descr:"Initial URI of a request."
    [("", Lang.request_t (Lang.univ_t ()), None, None)]
    Lang.string_t
    (fun p ->
      let r = Lang.to_request (List.assoc "" p) in
      Lang.string (Request.initial_uri r))

let () =
  add_builtin "request.filename" ~cat:Liq
    ~descr:
      "Return a valid local filename if the request is ready, and the empty \
       string otherwise."
    [("", Lang.request_t (Lang.univ_t ()), None, None)]
    Lang.string_t
    (fun p ->
      let r = Lang.to_request (List.assoc "" p) in
      Lang.string (match Request.get_filename r with Some f -> f | None -> ""))

let () =
  add_builtin "request.destroy" ~cat:Liq
    ~descr:
      "Destroying a request causes any temporary associated file to be \
       deleted, and releases its RID. Persistent requests resist to \
       destroying, unless forced."
    [ ( "force",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Destroy the request even if it is persistent." );
      ("", Lang.request_t (Lang.univ_t ()), None, None) ]
    Lang.unit_t
    (fun p ->
      let force = Lang.to_bool (List.assoc "force" p) in
      let e = Lang.to_request (List.assoc "" p) in
      Request.destroy ~force e ; Lang.unit)

let () =
  add_builtin "request.duration" ~cat:Liq [("", Lang.string_t, None, None)]
    Lang.float_t
    ~descr:
      "Compute the duration in seconds of audio data contained in a request. \
       The computation may be expensive. Returns -1. if computation failed, \
       typically if the file was not recognized as valid audio." (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.float (try Request.duration f with Not_found -> -1.))

let () =
  add_builtin "request.id" ~cat:Liq ~descr:"Identifier of a request."
    [("", Lang.request_t (Lang.univ_t ()), None, None)]
    Lang.int_t
    (fun p ->
      let r = Lang.to_request (List.assoc "" p) in
      Lang.int (Request.get_id r))
