(*****************************************************************************

   Liquidsoap, a programmable stream generator.
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

open Source
open Request_source

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "request.once" ~category:Lang.Input
    ~descr:"Play a request once and become unavailable."
    [
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 20.),
        Some "Timeout in seconds for resolving the request." );
      ("", Lang.request_t, None, Some "Request to play.");
    ]
    ~meth:
      [
        ( "resolve",
          ([], Lang.fun_t [] Lang.bool_t),
          "Resolve the request (this is useful to make sure that the source \
           will be available in advance). This function returns `true` if we \
           were able to successfully perform resolution.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.bool s#resolve) );
      ]
    ~return_t
    (fun p ->
      let timeout = List.assoc "timeout" p |> Lang.to_float in
      let r = List.assoc "" p |> Lang.to_request in
      let kind = Source.Kind.of_kind kind in
      new once ~kind ~name:"request.once" ~timeout r)

exception Invalid_URI of string

(** [r] must resolve and be always ready. *)
class unqueued ~kind ~timeout request =
  object (self)
    inherit Request_source.unqueued ~name:"single" ~kind as super

    method wake_up x =
      let uri = Request.initial_uri request in
      self#log#important "%S is static, resolving once for all..." uri;
      if
        Request.Resolved
        <> Request.resolve ~ctype:(Some self#ctype) request timeout
      then raise (Invalid_URI uri);
      let filename = Option.get (Request.get_filename request) in
      if String.length filename < 15 then self#set_id filename;
      super#wake_up x

    method stype = Infallible

    method get_next_file = Some request
  end

class queued ~kind uri length default_duration timeout conservative =
  object (self)
    inherit
      Request_source.queued
        ~name:"single" ~kind ~length ~default_duration ~conservative ~timeout () as super

    method wake_up x =
      if String.length uri < 15 then self#set_id uri;
      super#wake_up x

    method get_next_request = Some (self#create_request uri)
  end

let log = Log.make ["single"]

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "single" ~category:Lang.Input
    ~descr:
      "Loop on a request. It never fails if the request is static, meaning \
       that it can be fetched once. Typically, http, ftp, say requests are \
       static, and time is not."
    ( ("", Lang.string_t, None, Some "URI where to find the file")
    :: ( "fallible",
         Lang.bool_t,
         Some (Lang.bool false),
         Some "Enforce fallibility of the request." )
    :: queued_proto )
    ~return_t
    (fun p ->
      let val_uri = List.assoc "" p in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      let l, d, t, c = extract_queued_params p in
      let uri = Lang.to_string val_uri in
      let kind = Source.Kind.of_kind kind in
      if (not fallible) && Request.is_static uri then (
        let request = Request.create ~persistent:true uri in
        (new unqueued ~kind ~timeout:t request :> source) )
      else (new queued uri ~kind l d t c :> source))

let () =
  let kind = Lang.any in
  let t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "single.infallible" ~category:Lang.Input
    ~flags:[Lang.Hidden]
    ~descr:
      "Loops on a request, which has to be ready and should be persistent. \
       WARNING: if used uncarefully, it can crash your application!"
    [("", Lang.request_t, None, None)] ~return_t:t (fun p ->
      let request = Lang.to_request (List.assoc "" p) in
      let kind = Source.Kind.of_kind kind in
      (new unqueued ~kind ~timeout:60. request :> source))

class dynamic ~kind ~retry_delay ~available (f : Lang.value) length
  default_duration timeout conservative =
  object (self)
    inherit
      Request_source.queued
        ~kind ~name:"request.dynamic.list" ~length ~default_duration ~timeout
          ~conservative () as super

    val mutable retry_status = None

    method is_ready =
      match (super#is_ready, retry_status) with
        | true, _ -> true
        | false, Some d when Unix.gettimeofday () < d -> false
        | false, _ ->
            if available () then super#notify_new_request;
            false

    (* First cache last requests. *)
    val mutable last_requests = []

    method private get_next_requests =
      try
        if available () then (
          let reqs =
            List.map Lang.to_request (Lang.to_list (Lang.apply f []))
          in
          List.iter
            (fun req -> Request.set_root_metadata req "source" self#id)
            reqs;
          reqs )
        else []
      with e ->
        log#severe "Failed to obtain a media request!";
        raise e

    method get_next_request =
      match last_requests with
        | req :: tl ->
            last_requests <- tl;
            Some req
        | [] -> (
            match self#get_next_requests with
              | req :: tl ->
                  last_requests <- tl;
                  Some req
              | [] ->
                  retry_status <- Some (Unix.gettimeofday () +. retry_delay ());
                  None )
  end

let () =
  let log = Log.make ["request"; "dynamic"] in
  let kind = Lang.any in
  let t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "request.dynamic.list" ~category:Lang.Input
    ~descr:"Play request dynamically created by a given function."
    ( ("", Lang.fun_t [] (Lang.list_t Lang.request_t), None, None)
    :: ( "retry_delay",
         Lang.getter_t Lang.float_t,
         Some (Lang.float 0.1),
         Some
           "Retry after a given time (in seconds) when callback returns an \
            empty list." )
    :: ( "available",
         Lang.getter_t Lang.bool_t,
         Some (Lang.bool true),
         Some
           "Whether some new requests are available (when set to false, it \
            stops after current playing request)." )
    :: queued_proto )
    ~meth:
      [
        ( "prefetch",
          ([], Lang.fun_t [] Lang.unit_t),
          "Try feeding the queue with a new request (this method can take long \
           to return and should usually be run in a separate thread).",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#prefetch with
                  | `Finished -> Lang.unit
                  | `Retry ->
                      log#important "Prefetch failed: retry.";
                      Lang.unit
                  | `Empty ->
                      log#important "Prefetch failed: empty.";
                      Lang.unit) );
        ( "queue_size",
          ([], Lang.fun_t [] Lang.int_t),
          "Number of requests in the queue.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.int s#queue_size) );
        ( "queue_length",
          ([], Lang.fun_t [] Lang.float_t),
          "Size of the queue in seconds.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#queue_length) );
      ]
    ~return_t:t
    (fun p ->
      let f = List.assoc "" p in
      let available = Lang.to_bool_getter (List.assoc "available" p) in
      let retry_delay = Lang.to_float_getter (List.assoc "retry_delay" p) in
      let l, d, t, c = extract_queued_params p in
      let kind = Source.Kind.of_kind kind in
      new dynamic ~kind ~available ~retry_delay f l d t c)
