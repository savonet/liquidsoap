
(** Tool for user request processing sources. *)

open Types

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
class external_request =
object (self)
  inherit Play_files.queued as queued

  val reqlock = Tutils.Mutex.create "External_request.reqlock"
  val requests = Queue.create ()

  method get_next_request =
    try
      Mutex.lock reqlock ;
      let out = Queue.take requests in
	ignore
	  (Queue.fold (fun pos req ->
			 let large,low = split pos in
			   Request.set_metadata req "2nd_queue_pos"
			     (string_of_int pos) ;
			   if large <= 5 && low = 0 then
			     Request.log req
			       (Printf.sprintf "#%d in secondary queue" pos) ;
			   (pos+1) ) 1 requests) ;
	Mutex.unlock reqlock ;
	if Request.get_metadata out "skip" = Some "true" then
	  ( Request.log out "Out of the secondary queue, but skipped." ;
	    Request.destroy out ;
	    self#get_next_request )
	else
	  ( Request.log out "Entering the primary queue." ;
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
           "%S entered the secondary queue : position #%d" i
           (Queue.length requests)) ;
      Mutex.unlock reqlock

  (** Get a copy of the resolved (primary) and unresolved (secondary) queues. *)
  method copy_queue =
    Mutex.lock reqlock ;
    let q = Queue.fold (fun l r -> r::l) [] requests in
      Mutex.unlock reqlock ;
      q@(queued#copy_queue)

end
