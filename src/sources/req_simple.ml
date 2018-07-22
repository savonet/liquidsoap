(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2018 Savonet team

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

(** [r] must resolve and be always ready. *)
class unqueued ~kind r =
object (self)
  inherit Request_source.unqueued ~name:"single" ~kind as super

  method wake_up x =
    self#log#f 3 "%S is static, resolving once for all..." (Request.initial_uri r) ;
    if Request.Resolved <> Request.resolve r 60. then
      raise Invalid_URI ;
    let filename = Utils.get_some (Request.get_filename r) in
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
       if Request.is_static uri then
         let r = Request.create ~kind ~persistent:true uri in
         ((new unqueued ~kind r) :> source)
       else
         ((new queued uri ~kind l d t c) :> source))

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
