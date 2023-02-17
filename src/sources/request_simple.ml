(*****************************************************************************

   Liquidsoap, a programmable stream generator.
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

open Source
open Request_source

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "request.once" ~category:`Input
    ~descr:"Play a request once and become unavailable."
    [
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 20.),
        Some "Timeout in seconds for resolving the request." );
      ("", Builtins_request.Value.t, None, Some "Request to play.");
    ]
    ~meth:
      [
        ( "resolve",
          ([], Lang.fun_t [] Lang.bool_t),
          "Resolve the request (this is useful to make sure that the source \
           will be available in advance). This function returns `true` if we \
           were able to successfully perform resolution. You should use this \
           method instead of `request.resolve` to make sure that the proper \
           content type is decoded from the request.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.bool s#resolve) );
        ( "request",
          ([], Builtins_request.Value.t),
          "Get the request played by this source",
          fun s -> Builtins_request.Value.to_value s#request );
      ]
    ~return_t
    (fun p ->
      let timeout = List.assoc "timeout" p |> Lang.to_float in
      let r = List.assoc "" p |> Builtins_request.Value.of_value in
      let kind = Kind.of_kind kind in
      new once ~kind ~name:"request.once" ~timeout r)

exception Invalid_URI of string

(** [r] must resolve and be always ready. *)
class unqueued ~kind ~timeout request =
  object (self)
    inherit Request_source.unqueued ~name:"single" ~kind as super

    method wake_up x =
      let uri = Request.initial_uri request in
      self#log#important "%s is static, resolving once for all..."
        (Utils.quote_string uri);
      if
        Request.Resolved
        <> Request.resolve ~ctype:(Some self#ctype) request timeout
      then raise (Invalid_URI uri);
      let filename = Option.get (Request.get_filename request) in
      if String.length filename < 15 then (
        try self#set_id filename
        with _ ->
          ();
          super#wake_up x)

    method stype = `Infallible
    method get_next_file = `Request request
  end

class queued ~kind uri prefetch timeout =
  object (self)
    inherit
      Request_source.queued ~name:"single" ~kind ~prefetch ~timeout () as super

    method wake_up x =
      if String.length uri < 15 then self#set_id uri;
      super#wake_up x

    method get_next_request = `Request (self#create_request uri)
  end

let log = Log.make ["single"]

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "single" ~category:`Input
    ~descr:
      "Loop on a request. It never fails if the request is static, meaning \
       that it can be fetched once. Typically, http, ftp, say requests are \
       static, and time is not."
    (("", Lang.string_t, None, Some "URI where to find the file")
    :: ( "fallible",
         Lang.bool_t,
         Some (Lang.bool false),
         Some "Enforce fallibility of the request." )
    :: queued_proto)
    ~return_t
    (fun p ->
      let val_uri = List.assoc "" p in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      let l, t = extract_queued_params p in
      let uri = Lang.to_string val_uri in
      let kind = Kind.of_kind kind in
      if (not fallible) && Request.is_static uri then (
        let request = Request.create ~persistent:true uri in
        (new unqueued ~kind ~timeout:t request :> source))
      else (new queued uri ~kind l t :> source))

let () =
  let kind = Lang.any in
  let t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "single.infallible" ~category:`Input ~flags:[`Hidden]
    ~descr:
      "Loops on a request, which has to be ready and should be persistent. \
       WARNING: if used uncarefully, it can crash your application!"
    [("", Builtins_request.Value.t, None, None)]
    ~return_t:t
    (fun p ->
      let request = Builtins_request.Value.of_value (List.assoc "" p) in
      let kind = Kind.of_kind kind in
      (new unqueued ~kind ~timeout:60. request :> source))

class dynamic ~kind ~retry_delay ~available (f : Lang.value) prefetch timeout =
  object (self)
    inherit
      Request_source.queued ~kind ~name:"request.dynamic" ~prefetch ~timeout () as super

    val mutable retry_status = None

    method is_ready =
      match (super#is_ready, retry_status) with
        | true, _ -> true
        | false, Some d when Unix.gettimeofday () < d -> false
        | false, _ ->
            if available () then super#notify_new_request;
            false

    method private get_next_request =
      let retry () =
        let delay = retry_delay () in
        retry_status <- Some (Unix.gettimeofday () +. delay);
        `Retry (fun () -> delay)
      in
      match
        ( available (),
          Lang.to_valued_option Builtins_request.Value.of_value
            (Lang.apply f []) )
      with
        | false, _ -> `Empty
        | true, Some r ->
            Request.set_root_metadata r "source" self#id;
            `Request r
        | true, None -> retry ()
        | exception exn ->
            let bt = Printexc.get_backtrace () in
            Utils.log_exception ~log:self#log ~bt
              (Printf.sprintf "Failed to obtain a media request: %s"
                 (Printexc.to_string exn));
            retry ()
  end

let () =
  let log = Log.make ["request"; "dynamic"] in
  let kind = Lang.any in
  let t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "request.dynamic" ~category:`Input
    ~descr:"Play request dynamically created by a given function."
    (("", Lang.fun_t [] (Lang.nullable_t Builtins_request.Value.t), None, None)
    :: ( "retry_delay",
         Lang.getter_t Lang.float_t,
         Some (Lang.float 0.1),
         Some
           "Retry after a given time (in seconds) when callback returns `null`."
       )
    :: ( "available",
         Lang.getter_t Lang.bool_t,
         Some (Lang.bool true),
         Some
           "Whether some new requests are available (when set to false, it \
            stops after current playing request)." )
    :: queued_proto)
    ~meth:
      [
        ( "fetch",
          ([], Lang.fun_t [] Lang.bool_t),
          "Try feeding the queue with a new request. Returns `true` if \
           successful. This method can take long to return and should usually \
           be run in a separate thread.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#fetch with
                  | `Finished -> Lang.bool true
                  | `Retry _ ->
                      log#important "Fetch failed: retry.";
                      Lang.bool false
                  | `Empty ->
                      log#important "Fetch failed: empty.";
                      Lang.bool false) );
        ( "queue",
          ([], Lang.fun_t [] (Lang.list_t Builtins_request.Value.t)),
          "Get the requests currently in the queue.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                Lang.list
                  (List.rev
                     (Queue.fold
                        (fun c i ->
                          Builtins_request.Value.to_value i.request :: c)
                        [] s#queue))) );
        ( "add",
          ([], Lang.fun_t [(false, "", Builtins_request.Value.t)] Lang.bool_t),
          "Add a request to the queue. Requests are resolved before being \
           added. Returns `true` if the request was successfully added.",
          fun s ->
            Lang.val_fun
              [("", "", None)]
              (fun p ->
                Lang.bool
                  (s#add
                     {
                       request =
                         Builtins_request.Value.of_value (List.assoc "" p);
                       expired = false;
                     })) );
        ( "set_queue",
          ( [],
            Lang.fun_t
              [(false, "", Lang.list_t Builtins_request.Value.t)]
              Lang.unit_t ),
          "Set the queue of requests. Requests are resolved before being added \
           to the queue. You are responsible for destroying the requests \
           currently in the queue.",
          fun s ->
            Lang.val_fun
              [("", "", None)]
              (fun p ->
                let l =
                  List.map Builtins_request.Value.of_value
                    (Lang.to_list (List.assoc "" p))
                in
                let q = Queue.create () in
                List.iter
                  (fun request -> Queue.push { request; expired = false } q)
                  l;
                s#set_queue q;
                Lang.unit) );
        ( "current",
          ([], Lang.fun_t [] (Lang.nullable_t Builtins_request.Value.t)),
          "Get the request currently being played.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#current with
                  | None -> Lang.null
                  | Some c -> Builtins_request.Value.to_value c.req) );
      ]
    ~return_t:t
    (fun p ->
      let f = List.assoc "" p in
      let available = Lang.to_bool_getter (List.assoc "available" p) in
      let retry_delay = Lang.to_float_getter (List.assoc "retry_delay" p) in
      let l, t = extract_queued_params p in
      let kind = Kind.of_kind kind in
      new dynamic ~kind ~available ~retry_delay f l t)
