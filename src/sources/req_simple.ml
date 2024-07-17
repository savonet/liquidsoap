(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class unqueued r =
  (** We assume that [r] is ready. *)
  let filename = Utils.get_some (Request.get_filename r) in
object (self)
  inherit Request_source.unqueued

  method wake_up _ =
    if String.length filename < 15 then
      self#set_id filename

  method stype = Infallible
  method get_next_file = Some r
end

class queued uri length default_duration timeout =
object (self)
  inherit Request_source.queued ~length ~default_duration ~timeout ()
  method get_next_request = self#create_request uri
end

let log = Log.make ["single"]

let () =
  Lang.add_operator "single"
    ~category:Lang.Input
    ~descr:("Loop on a request. It never fails if the request "^
            "is static, meaning that it can be fetched once. "^
            "Typically, http, ftp, say requests are static, "^
            "and time is not.")
    (( "", Lang.string_t, None, Some "URI where to find the file" )::
     queued_proto)
    (fun p ->
       let val_uri = List.assoc "" p in
       let l,d,t = extract_queued_params p in
       let uri = Lang.to_string val_uri in
         try match
           (* Being static is not enough when the single() is being
            * dynamically created: the creation must be immediate.
            * For example, speech synthesis is static but long.
            * Finally, the unqueued source asserts that it can create a request,
            * which is not realistic in the context of dynamic instantiation,
            * where there may temporarily be no RID left. *)
           if Root.running () then None else Request.is_static uri
         with
           | Some true ->
               begin match Request.create ~persistent:true uri with
                 | None ->
                     (* This is only ran at startup, it's very unlikely
                      * that we ever run out of RID there... *)
                     log#f 2 "No available RID: %S will be queued.." uri ;
                     ((new queued uri l d t) :> source)
                 | Some r ->
                     log#f 3 "%S is static, resolving once for all..." uri ;
                     if Request.Resolved <> Request.resolve r 60. then
                       raise Invalid_URI ;
                     ((new unqueued r) :> source)
               end
           | None | Some false ->
               log#f 3 "%S will be queued." uri ;
               ((new queued uri l d t) :> source)
         with
           | Invalid_URI ->
               raise (Lang.Invalid_value
                        (val_uri,
                         (Printf.sprintf
                            "Could not get a valid audio file from %S"
                            uri))))

let () =
  Lang.add_operator "unsafe.single.infallible" ~category:Lang.Input
    ~flags:[Lang.Hidden]
    ~descr:("Loops on a request, "^
            "which has to be ready and should be persistent. "^
            "WARNING: if used uncarefully, it can crash your application!")
    [ "", Lang.request_t, None, None]
    (fun p ->
       let r = Utils.get_some (Lang.to_request (List.assoc "" p)) in
         ((new unqueued r):>source))

class dynamic (f:Lang.value) length default_duration timeout =
object (self)
  inherit Request_source.queued ~length ~default_duration ~timeout ()
  method get_next_request =
    match Lang.to_request (Lang.eval (Lang.apply f [])) with
      | None -> None
      | Some req ->
          Request.set_root_metadata req "source" self#id ;
          Some req
end

let () =
  Lang.add_operator "request.dynamic" ~category:Lang.Input
    ~descr:"Play request dynamically created by a given function."
    (( "", Lang.fun_t [] Lang.request_t, None,
       Some ("A function generating requests: an initial URI (possibly fake)" ^
             " together with an initial list of alternative indicators."))
     ::queued_proto)
    (fun p ->
       let f = List.assoc "" p in
       let l,d,t = extract_queued_params p in
         ((new dynamic f l d t) :> source))
