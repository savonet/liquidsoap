(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
class equeue length default_duration timeout =
object (self)
  inherit Request_source.queued ~length ~default_duration ~timeout () as super

  val queue = Rqueue.create ()

  method get_next_request =
    try
      let r = Rqueue.shift queue in
      Request.add_log r "Entering the primary queue." ;
      Request.set_root_metadata r "queue" "primary" ;
      Some r
    with
      | Rqueue.Not_found ->  None

  val mutable ns = []

  method copy_queue_init q =
    Rqueue.fold
      (fun l r -> r::l)
      q
      queue

  method copy_queue = self#copy_queue_init super#copy_queue

  method wake_up activation =
    super#wake_up activation ;
    if ns = [] then
      ns <- Server.register [self#id] "editable" ;
    let make_n_add f req =
      match self#create_request req with
      | Some req ->
          f req ;
          Request.add_log req "Entering the secondary queue." ;
          Request.set_root_metadata req "queue" "secondary" ;
          self#notify_new_request ;
          (string_of_int (Request.get_id req))
      | None -> "Unable to create a request!"
    in
      Server.add ~ns "push" ~usage:"push <uri>"
                 ~descr:"Push a new request in the queue."
        (make_n_add (Rqueue.push queue)) ;
    let print_queue q = 
           String.concat " "
             (List.map
                (fun r -> string_of_int (Request.get_id r))
                (List.rev q))
    in
      Server.add ~ns "queue"
        (fun _ -> print_queue self#copy_queue)
                 ~descr:"Display current queue content for \
		         both primary and secondary queues.";
      Server.add ~ns "primary_queue"
        (fun _ -> print_queue super#copy_queue)
                 ~descr:"Display current queue content for \
                         the primary queue.";
      Server.add ~ns "secondary_queue"
        (fun _ -> print_queue (self#copy_queue_init []))
                 ~descr:"Display current queue content for \
                         the secondary queue.";
      (* Since the queue command gives not only the pending queue,
       * it can be useful to have the size of our queue *)
      Server.add ~ns "pending_length" ~descr:"Return the length of the secondary queue."
        (fun _ ->
           string_of_int (Rqueue.length queue)) ;
      Server.add ~ns "insert" ~usage:"insert <pos> <uri>"
        (fun a ->
           if Str.string_match insert_re a 0 then
             let pos = int_of_string (Str.matched_group 1 a) in
             let uri = Str.matched_group 2 a in
               make_n_add (Rqueue.insert queue pos) uri
           else
             "Usage: insert <pos> <uri>") 
	        ~descr:"Insert <uri> at position <pos> in the secondary queue." ;
      Server.add ~ns "remove" ~usage:"remove <rid>"
        (fun a ->
           let id = int_of_string a in
             try
               let req =
                 Rqueue.remove_pred queue (fun _ r -> Request.get_id r = id)
               in
                 Request.destroy req ;
                 "OK"
             with
             | Rqueue.Not_found -> "No such request in my queue") 
	        ~descr:"Remove request <rid> from the secondary queue." ;
      Server.add ~ns "move" ~usage:"move <rid> <pos>" 
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

  method private sleep =
    super#sleep ;
    try
      while true do
        Request.destroy (Rqueue.pop queue)
      done
    with Queue.Empty -> ()

end

let () =
  Lang.add_operator "request.equeue"
    ~category:Lang.Input
    ~descr:("Receive URIs from users, and play them. "^
            "Insertion and deletion possible at any position.")
    Request_source.queued_proto
    (fun p -> let l,d,t = Request_source.extract_queued_params p in
                ((new equeue l d t) :> source))
