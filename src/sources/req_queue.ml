(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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
class queue ~kind
  ?(requests=Queue.create()) ?(interactive=true)
  length default_duration timeout conservative =
object (self)
  inherit Request_source.queued ~kind ~name:"request.queue"
            ~length ~default_duration ~timeout ~conservative () as queued

  initializer
    if interactive then begin
      self#set_id ~definitive:false "queue" ;
      ns_kind <- "queue" ;
      self#register_command "push" ~usage:"push <uri>"
        ~descr:"Push a new request in the queue."
        (fun req ->
           let req = self#create_request req in
           let id = Request.get_id req in
             Request.set_root_metadata req "source_id"
               (string_of_int (Oo.id self)) ;
             self#push_request req ;
             string_of_int id) ;
      let print_queue q =
        String.concat " "
          (List.map
             (fun r -> string_of_int (Request.get_id r))
             (List.rev q))
      in
      self#register_command "queue"
        ~descr:"Display current queue content for both primary and \
                secondary queue."
        (fun _ -> print_queue self#copy_queue) ;
      self#register_command "primary_queue"
        ~descr:"Display current queue content for the primary queue."
        (fun _ -> print_queue queued#copy_queue) ;
      self#register_command "secondary_queue"
        ~descr:"Display current queue content for the seconary queue."
        (fun _ -> print_queue (self#copy_queue_init [])) ;
      self#register_command "ignore" ~usage:"ignore <rid>"
        ~descr:"Indicate that request <rid> should not be played \
                (set the \"skip\" metadata to true)."
        (fun s ->
           let id = int_of_string s in
             match Request.from_id id with
               | None -> "ERROR: No such request!"
               | Some r ->
                   if Request.get_root_metadata r "source_id" <>
                        Some (string_of_int (Oo.id self)) then
                     "ERROR: That request doesn't belong to me!"
                   else if Request.is_on_air r then
                     "ERROR: Request already on air!"
                   else if Request.get_metadata r "queue" = Some "primary" then begin
                     self#expire (fun r' -> Request.get_id r = Request.get_id r') ;
                     "OK"
                   end else begin
                     Request.set_root_metadata r "skip" "true" ;
                     "OK"
                   end) ;
      self#register_command "consider" ~usage:"consider <rid>"
        ~descr:"Cancel the effect of ignore on a request."
        (fun s ->
           let id = int_of_string s in
             match Request.from_id id with
               | None -> "No such request!"
               | Some r ->
                   if Request.get_root_metadata r "source_id" <>
                        Some (string_of_int (Oo.id self)) then
                     "ERROR: That request doesn't belong to me!"
                   else if Request.get_metadata r "queue" = Some "primary" then begin
                     "ERROR: Cannot un-ignore in the primary queue."
                   end else begin
                     Request.set_root_metadata r "skip" "false" ;
                     "OK"
                   end)
      end



  val reqlock = Mutex.create ()

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
                             Request.add_log req
                               (Printf.sprintf "#%d in secondary queue" pos) ;
                           (pos+1) ) 1 requests) ;
        Mutex.unlock reqlock ;
        if Request.get_root_metadata out "skip" = Some "true" then begin
          Request.add_log out "Out of the secondary queue, but skipped." ;
          Request.destroy out ;
          self#get_next_request
        end else begin
          Request.add_log out "Entering the primary queue." ;
          Request.set_root_metadata out "2nd_queue_pos" "0" ;
          Request.set_root_metadata out "queue" "primary" ;
          Some out
        end
    with
      | Queue.Empty -> Mutex.unlock reqlock ; None

  (** Simply push a request here to have it played. *)
  method push_request req =
    let i = Request.peek_indicator req in
      Mutex.lock reqlock ;
      Queue.add req requests ;
      Request.add_log req
        (Printf.sprintf
           "%S entered the secondary queue : position #%d." i
           (Queue.length requests)) ;
      Request.set_root_metadata req "2nd_queue_pos"
        (string_of_int (Queue.length requests)) ;
      Request.set_root_metadata req "queue" "secondary" ;
      Mutex.unlock reqlock ;
      self#notify_new_request

  method copy_queue_init q =
    Mutex.lock reqlock ;
    let q = Queue.fold (fun l r -> r::l) q requests in
      Mutex.unlock reqlock ;
      q

  (** Get a copy of the resolved (primary) and unresolved (secondary) queues. *)
  method copy_queue = self#copy_queue_init queued#copy_queue

  method private sleep =
    queued#sleep ;
    try
      while true do
        Request.destroy (Queue.take requests)
      done
    with Queue.Empty -> ()

end

let () =
  let k = Lang.univ_t 1 in
  Lang.add_operator "request.queue"
    ~category:Lang.Input
    ~descr:"Receive URIs from users, and play them."
    (("queue",Lang.list_t (Lang.request_t k),
      Some (Lang.list ~t:(Lang.request_t k) []),
      Some "Initial queue of requests.")::
     ("interactive",Lang.bool_t,
      Some (Lang.bool true),
      Some "Should the queue be controllable via telnet?")::
     Request_source.queued_proto)
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
       let l,d,t,c = Request_source.extract_queued_params p in
       let interactive = Lang.to_bool (Lang.assoc "interactive" 1 p) in
       let requests = Queue.create () in
         List.iter
           (fun r -> Queue.add (Lang.to_request r) requests)
           (Lang.to_list (List.assoc "queue" p)) ;
         ((new queue ~kind ~requests ~interactive l d t c) :> source))
