(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2014 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source
open Request_source
open Dtools

exception Invalid_URI

class unqueued ~kind r =
  (** We assume that [r] is ready. *)
  let filename = Utils.get_some (Request.get_filename r) in
object (self)
  inherit Request_source.unqueued ~name:"single" ~kind as super

  method wake_up x =
    if String.length filename < 15 then self#set_id filename ;
    super#wake_up x

  method stype = Infallible
  method get_next_file = Some r
end

class queued ~kind uri length default_duration timeout conservative =
object (self)
  inherit Request_source.queued ~name:"single" ~kind
      ~length ~default_duration ~conservative ~timeout () as super

  method wake_up x =
    if String.length uri < 15 then self#set_id uri ;
    super#wake_up x

  method get_next_request = Some (self#create_request uri)
end

let log = Log.make ["single"]

let () =
  Lang.add_operator "single"
    ~category:Lang.Input
    ~descr:"Loop on a request. It never fails if the request \
            is static, meaning that it can be fetched once. \
            Typically, http, ftp, say requests are static, \
            and time is not."
    (( "", Lang.string_t, None, Some "URI where to find the file" )::
     queued_proto)
    ~kind:(Lang.Unconstrained (Lang.univ_t 1))
    (fun p kind ->
       let val_uri = List.assoc "" p in
       let l,d,t,c = extract_queued_params p in
       let uri = Lang.to_string val_uri in
         try match
           (* Being static is not enough when the single() is being
            * dynamically created: the creation must be immediate.
            * For example, speech synthesis is static but long.
            *
            * The compromise here is to attempt resolution only in
            * the initial loading phase, not in dynamically executed code.
            * This is not such a solid design choice: we don't take the
            * same precaution with other operators that take long to
            * create or wake up, such as playlist.safe(). *)
           if Clock.running () then None else Request.is_static uri
         with
           | Some true ->
               let r = Request.create ~kind ~persistent:true uri in
                 log#f 3 "%S is static, resolving once for all..." uri ;
                 if Request.Resolved <> Request.resolve r 60. then
                   raise Invalid_URI ;
                 ((new unqueued ~kind r) :> source)
           | None | Some false ->
               log#f 3 "%S will be queued." uri ;
               ((new queued uri ~kind l d t c) :> source)
         with
           | Invalid_URI ->
               raise (Lang.Invalid_value
                        (val_uri,
                         (Printf.sprintf
                            "Could not get a valid media file of kind \
                             %s from %S"
                            (Frame.string_of_content_kind kind)
                            uri))))

let () =
  let k = Lang.univ_t 1 in
  Lang.add_operator "unsafe.single.infallible" ~category:Lang.Input
    ~flags:[Lang.Hidden]
    ~descr:"Loops on a request, \
            which has to be ready and should be persistent. \
            WARNING: if used uncarefully, it can crash your application!"
    [ "", Lang.request_t k, None, None ]
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
       let r = Lang.to_request (List.assoc "" p) in
         ((new unqueued ~kind r):>source))

class dynamic ~kind
  (f:Lang.value) length default_duration timeout conservative =
object (self)
  inherit
    Request_source.queued ~kind ~name:"request.dynamic"
      ~length ~default_duration ~timeout ~conservative ()

  method get_next_request =
    try
      let t = Lang.request_t (Lang.kind_type_of_frame_kind kind) in
      let req = Lang.to_request (Lang.apply ~t f []) in
        Request.set_root_metadata req "source" self#id ;
        Some req
    with
      | e ->
          log#f 2 "Failed to obtain a media request!" ;
          raise e
end

let () =
  let k = Lang.univ_t 1 in
  Lang.add_operator "request.dynamic" ~category:Lang.Input
    ~descr:"Play request dynamically created by a given function."
    (( "", Lang.fun_t [] (Lang.request_t k), None, None)
     ::queued_proto)
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
       let f = List.assoc "" p in
       let l,d,t,c = extract_queued_params p in
         ((new dynamic ~kind f l d t c) :> source))
