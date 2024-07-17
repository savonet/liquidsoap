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

(** [split x] returns [h,l] where x = h*10^k + l, 0<h<=9, and k maximal. *)
let split x =
  let log10 x = int_of_float (log10 (float x)) in
  let pow10 x = int_of_float (10. ** (float x)) in
  let n = pow10 (log10 x) in
  let larger = x / n in
    larger, (x-(larger*n))

(** On the top of [queued] we add another queue, in which the unresolved
  * requests are stored. These requests can typically be pushed when some
  * user emits a request. *)
class queue length default_duration timeout =
object (self)
  inherit Request_source.queued ~length ~default_duration ~timeout () as queued

  val reqlock = Mutex.create ()
  val requests = Queue.create ()

  method get_next_request =
    try
      Mutex.lock reqlock ;
      let out = Queue.take requests in
	ignore
	  (Queue.fold (fun pos req ->
			 let large,low = split pos in
			   Request.set_root_metadata req "2nd_queue_pos"
			     (string_of_int pos) ;
			   if large <= 5 && low = 0 then
			     Request.log req
			       (Printf.sprintf "#%d in secondary queue" pos) ;
			   (pos+1) ) 1 requests) ;
	Mutex.unlock reqlock ;
	if Request.get_root_metadata out "skip" = Some "true" then
	  ( Request.log out "Out of the secondary queue, but skipped." ;
	    Request.destroy out ;
	    self#get_next_request )
	else
	  ( Request.log out "Entering the primary queue." ;
            Request.set_root_metadata out "2nd_queue_pos" "0" ;
	    Some out )
    with
      | Queue.Empty -> Mutex.unlock reqlock ; None

  (** Simply push a request here to have it played. *)
  method push_request req =
    let i = Request.peek_indicator req in
      Mutex.lock reqlock ;
      Queue.add req requests ;
      Request.log req
        (Printf.sprintf
           "%S entered the secondary queue : position #%d." i
           (Queue.length requests)) ;
      Request.set_root_metadata req "2nd_queue_pos"
        (string_of_int (Queue.length requests)) ;
      Mutex.unlock reqlock ;
      self#notify_new_request

  (** Get a copy of the resolved (primary) and unresolved (secondary) queues. *)
  method copy_queue =
    Mutex.lock reqlock ;
    let q = Queue.fold (fun l r -> r::l) queued#copy_queue requests in
      Mutex.unlock reqlock ;
      q

  val mutable ns = []

  method wake_up activation =
    queued#wake_up activation ;
    if ns = [] then
      ns <- Server.register [self#id] "queue" ;
    self#set_id (Server.to_string ns) ;
    Server.add ~ns "push" ~usage:"push <uri>"
      (fun req ->
         match self#create_request req with
         | Some req ->
             let id = Request.get_id req in
               Request.set_root_metadata req "source_id"
                 (string_of_int (Oo.id self)) ;
               self#push_request req ;
               (string_of_int id)
         | None -> "Unable to create a request!" ) ;
    Server.add ~ns "queue"
      (fun _ ->
         String.concat " "
           (List.map
              (fun r -> string_of_int (Request.get_id r))
              (List.rev self#copy_queue))) ;
    Server.add ~ns "ignore" ~usage:"ignore <rid>"
      (fun s ->
         ( let id = int_of_string s in
             match Request.from_id id with
             | None -> "No such request!"
             | Some r ->
                 if Request.get_root_metadata r "source_id" <>
                      Some (string_of_int (Oo.id self)) then
                   "That request doesn't belong to me!"
                 else
                   ( Request.set_root_metadata r "skip" "true" ;
                     "OK" ))) ;
    Server.add ~ns "consider" ~usage:"consider <rid>"
      (fun s ->
         ( let id = int_of_string s in
             match Request.from_id id with
             | None -> "No such request!"
             | Some r ->
                 if Request.get_root_metadata r "source_id" <>
                      Some (string_of_int (Oo.id self)) then
                   "That request doesn't belong to me!"
                 else
                   ( Request.set_root_metadata r "skip" "false" ;
                     "OK" )))
end

let _ =
  Lang.add_operator "request.queue"
    ~descr:"Receive URIs from users, and play them."
    Request_source.queued_proto
    (fun p -> let l,d,t = Request_source.extract_queued_params p in
                ((new queue l d t) :> source))
