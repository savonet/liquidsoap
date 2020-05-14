(*****************************************************************************

   Liquidsoap, a programmable stream generator.
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

open Source
open Request_source

exception Invalid_URI of string

(** [r] must resolve and be always ready. *)
class unqueued ~kind r =
  object (self)
    inherit Request_source.unqueued ~name:"single" ~kind as super

    val mutable request = None

    method request =
      match request with
        | Some request -> request
        | None ->
            let r = r self#ctype in
            request <- Some r;
            r

    method wake_up x =
      let request = self#request in
      let uri = Request.initial_uri request in
      self#log#important "%S is static, resolving once for all..." uri;
      if Request.Resolved <> Request.resolve request 60. then
        raise (Invalid_URI uri);
      let filename = Utils.get_some (Request.get_filename request) in
      if String.length filename < 15 then self#set_id filename;
      super#wake_up x

    method stype = Infallible

    method get_next_file = Some self#request
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
      if (not fallible) && Request.is_static uri then (
        let r ctype = Request.create ~ctype ~persistent:true uri in
        (new unqueued ~kind r :> source) )
      else (new queued uri ~kind l d t c :> source))

let () =
  let kind = Lang.any in
  let t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "unsafe.single.infallible" ~category:Lang.Input
    ~flags:[Lang.Hidden]
    ~descr:
      "Loops on a request, which has to be ready and should be persistent. \
       WARNING: if used uncarefully, it can crash your application!"
    [("", Lang.request_t t, None, None)]
    ~return_t:t
    (fun p ->
      let r _ = Lang.to_request (List.assoc "" p) in
      (new unqueued ~kind r :> source))

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
  let kind = Lang.any in
  let t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "request.dynamic.list" ~category:Lang.Input
    ~descr:"Play request dynamically created by a given function."
    ( ("", Lang.fun_t [] (Lang.list_t (Lang.request_t t)), None, None)
    :: ( "retry_delay",
         Lang.float_getter_t (),
         Some (Lang.float 0.1),
         Some
           "Retry after a given time (in seconds) when callback returns an \
            empty list." )
    :: ( "available",
         Lang.bool_getter_t (),
         Some (Lang.bool true),
         Some
           "Whether some new requests are available (when set to false, it \
            stops after current playing request)." )
    :: queued_proto )
    ~return_t:t
    (fun p ->
      let f = List.assoc "" p in
      let available = Lang.to_bool_getter (List.assoc "available" p) in
      let retry_delay = Lang.to_float_getter (List.assoc "retry_delay" p) in
      let l, d, t, c = extract_queued_params p in
      (new dynamic ~kind ~available ~retry_delay f l d t c :> source))
