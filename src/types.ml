
open Dtools

type source_t = Fallible | Infallible

(* Convention : -1 means Infinity, time unit is the frame. *)

class virtual source =
object (self)

  (** Source ids are computed while safety-typing. 
    * See types.mli for help on that. *) 

  val mutable id = "(nobody)"
  method id = id
  val mutable has_id = false
  method set_name s =
    has_id <- true ;
    id <- s

  initializer 
    id <- ("(nobody."^(string_of_int (Oo.id self))^")")

  (** Basic methods
    *
    * may be redefined in derived classes in most of the cases. *)

  method remaining = -1        (* Number of frames left in the current track *)

  method virtual is_ready : bool
  method virtual get : Mixer.Buffer.t -> unit
  method virtual abort_track : unit

  (** Called when the source must be ready and had no operator,
    * means that the source has to initialize. *)
  method virtual wake_up : unit

  (** When the last operator leaves the source.
    * Means that the source can free the ressources she needs. *)
  method virtual sleep : unit

  (** The next methods SHOULD NOT be redefined ... *)

  val mutable activators_num = 0
  val activators = Hashtbl.create 10
  val source_lock = Tutils.Mutex.create "Types.source_lock"

  method get_ready (op:operator) =
    (Tutils.mutexify source_lock
       (fun op ->
	  let must_wake_up = activators_num = 0 in
	    if not (Hashtbl.mem activators op)
	    then begin
	      activators_num <- activators_num +1 ;
	      Hashtbl.add activators op () ;
	    end ;
	    if must_wake_up then
	      ( Log.logl ~label:"status" 3 
		  (lazy (Log.f "%s wakes up" id)) ;
		self#wake_up ; )
       ) 
    ) op

  method leave (op:operator) =
    (Tutils.mutexify source_lock
       (fun op ->
	  if Hashtbl.mem activators op
	  then ( activators_num <- activators_num - 1 ;
		 Hashtbl.remove activators op ;
		 if activators_num = 0 then (
		   self#sleep ;
		   Log.logl ~label:"status" 3 
		     (lazy (Log.f "%s is going to bed" id)) ;
		 ))
       )
    ) op

  (** Typing stuff : see types.mli. *)

  method stype = Fallible

  val mutable activation_intervals = Scheduling.compiled_never ()
  val mutable number_of_fathers = 0

  method set_id i =
    number_of_fathers <- number_of_fathers + 1 ;
    if not has_id then 
      ( has_id <- true ;
	id <- i )

  method add_activation_interval i =
    let conflicts =
      Scheduling.compiled_never ()
      <> Scheduling.conjunction i activation_intervals
    in
      if conflicts then
	Printf.fprintf stderr
	  "[WW] Source %s may be unsafe !\n"
	  id ;
      activation_intervals <- Scheduling.disjunction i activation_intervals ;
      not (conflicts)

  (** Utils. *)

  (** Creates an audio request and sets the field 'source' to the relevant id
    in the metadatas of the request. *)
  method create_request url =
    match Request.create url with
      | None -> None
      | Some r ->
	  Request.set_metadata r "source" self#id ;
	  Some r

end



(*****************************************************************************)
(* Operator                                                                  *)
(*****************************************************************************)

and virtual operator (sources:source list) =
object (self)
  inherit source as super

  val mutable fathers_done = 0

  method propagate_typing =
    List.fold_left
      (fun ok s -> 
	 (s#add_activation_interval activation_intervals) && ok)
      true sources

  method add_activation_interval i =
    let no_conflict = super#add_activation_interval i in
      fathers_done <- fathers_done + 1 ;
      if fathers_done = number_of_fathers then
	self#propagate_typing && no_conflict
      else
	no_conflict

  val mutable propagated_id = false

  method set_id i =
    number_of_fathers <- number_of_fathers + 1 ;
    if not has_id then self#set_name i ;
    if not propagated_id then (
      propagated_id <- true ;
      ignore (List.fold_left 
		(fun i s -> s#set_id (id^"."^(string_of_int i)) ; i+1)
		1 sources ) )

end
