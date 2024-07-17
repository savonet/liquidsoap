(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

class unqueued uri =
object (self)
  inherit Request_source.unqueued

  val mutable req = None

  initializer
  let file = match self#create_request ~persistent:true uri with
    | None -> assert false
    | Some r -> r
  in
    if Request.Resolved <> Request.resolve file 60. then raise Invalid_URI ;
    req <- Some file

  method private wake_up _ =
    let id = Filename.basename uri in
      if String.length id < 15 then
        self#set_id id

  method stype = Infallible
  method get_next_file = req
end

class queued uri length default_duration timeout =
object (self)
  inherit Request_source.queued ~length ~default_duration ~timeout ()
  method get_next_request = self#create_request uri
end

let log n l = Log.log ~label:"one_file" n l

let _ =
  Lang.add_operator "single"
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
         try match Request.is_static uri with
           | Some true ->
               log 3 (Log.f "%S doesn't need a queue." uri) ;
               ((new unqueued uri) :> source)
           | None | Some false ->
               log 3 (Log.f "%S will be queued." uri) ;
               ((new queued uri l d t) :> source)
         with
           | Invalid_URI ->
               raise (Lang.Invalid_value
                        (val_uri,
                         (Printf.sprintf
                            "Could not get a valid audio file from %S"
                            uri))))

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

let _ =
  Lang.add_operator "request.dynamic"
    ~descr:"Play request dynamically created by a given function."
    (( "", Lang.fun_t [] Lang.request_t, None,
       Some ("A function generating requests: an initial URI (possibly fake)" ^
             " together with an initial list of alternative indicators"))
     ::queued_proto)
    (fun p ->
       let f = List.assoc "" p in
       let l,d,t = extract_queued_params p in
         ((new dynamic f l d t) :> source))
