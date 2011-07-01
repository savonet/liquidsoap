(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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
open Unix
open Dtools

let insert_re = Str.regexp "\\(-?[0-9]+\\) +\\(.*\\)"
let move_re = Str.regexp "\\([0-9]+\\) +\\(-?[0-9]+\\)"

(** [telnet_request] is an [external_request] which queue is feed by
  * requests made over the network using the Server interface. *)
class equeue ~kind length default_duration timeout conservative =
object (self)
  inherit Request_source.queued ~kind ~name:"request.equeue"
            ~length ~default_duration ~timeout ~conservative () as super

  val queue = Rqueue.create ()

  initializer
    ns_kind <- "editable" ;
    let make_n_add f req =
      let req = self#create_request req in
        f req ;
        Request.add_log req "Entering the secondary queue." ;
        Request.set_root_metadata req "queue" "secondary" ;
        self#notify_new_request ;
        string_of_int (Request.get_id req)
    in
    let print_queue q =
      String.concat " "
        (List.map
           (fun r -> string_of_int (Request.get_id r))
           (List.rev q))
    in
      self#register_command "push" ~usage:"push <uri>"
                 ~descr:"Push a new request in the queue."
        (make_n_add (Rqueue.push queue)) ;
      self#register_command "queue"
        ~descr:"Display current queue content for \
                both primary and secondary queues."
        (fun _ -> print_queue self#copy_queue) ;
      self#register_command "primary_queue"
        ~descr:"Display current queue content for the primary queue."
        (fun _ -> print_queue super#copy_queue) ;
      self#register_command "secondary_queue"
        ~descr:"Display current queue content for the secondary queue."
        (fun _ -> print_queue (self#copy_queue_init [])) ;
      (* Since the queue command gives not only the pending queue,
       * it can be useful to have the size of our queue *)
      self#register_command "pending_length"
        ~descr:"Return the length of the secondary queue."
        (fun _ ->
           string_of_int (Rqueue.length queue)) ;
      self#register_command "insert" ~usage:"insert <pos> <uri>"
        ~descr:"Insert <uri> at position <pos> in the secondary queue."
        (fun a ->
           if Str.string_match insert_re a 0 then
             let pos = int_of_string (Str.matched_group 1 a) in
             let uri = Str.matched_group 2 a in
               make_n_add (Rqueue.insert queue pos) uri
           else
             "Usage: insert <pos> <uri>") ;
      self#register_command "remove" ~usage:"remove <rid>"
        ~descr:"Remove request <rid> from the secondary queue."
        (fun a ->
           let id = int_of_string a in
             try
               let req =
                 Rqueue.remove_pred queue (fun _ r -> Request.get_id r = id)
               in
                 Request.destroy req ;
                 "OK"
             with
             | Rqueue.Not_found -> "No such request in my queue") ;
      self#register_command "move" ~usage:"move <rid> <pos>"
        ~descr:"Move request <rid> in the secondary queue."
        (fun a ->
           if Str.string_match move_re a 0 then
             let rid = int_of_string (Str.matched_group 1 a) in
             let pos = int_of_string (Str.matched_group 2 a) in
               try
                 let req,i = Rqueue.remove_pred_index queue
                             (fun _ r -> Request.get_id r = rid)
                 in
                   try
                     Rqueue.insert queue pos req ;
                     "OK"
                   with
                   | Rqueue.Not_found ->
                       Request.destroy req ;
                       "Insertion failed, request lost"
               with
               | Rqueue.Not_found -> "No such request in my queue"
           else
             "Usage: move <rid> <pos>")

  method get_next_request =
    try
      let r = Rqueue.shift queue in
      Request.add_log r "Entering the primary queue." ;
      Request.set_root_metadata r "queue" "primary" ;
      Some r
    with
      | Rqueue.Not_found ->  None

  method copy_queue_init q =
    Rqueue.fold
      (fun l r -> r::l)
      q
      queue

  method copy_queue = self#copy_queue_init super#copy_queue

  method private sleep =
    super#sleep ;
    try
      while true do
        Request.destroy (Rqueue.pop queue)
      done
    with Rqueue.Not_found -> ()

end

let () =
  Lang.add_operator "request.equeue"
    ~category:Lang.Input
    ~descr:"Receive URIs from users, and play them. \
            Insertion and deletion possible at any position."
    Request_source.queued_proto
    ~kind:(Lang.Unconstrained (Lang.univ_t 1))
    (fun p kind ->
       let l,d,t,c = Request_source.extract_queued_params p in
         ((new equeue ~kind l d t c) :> source))
