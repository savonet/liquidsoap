
(** Abstract operator for select one of their sources for every new track,
  * depending on a parametrizable predicate.
  * A few instances of it are defined. *)

open Types
open Dtools

exception Found of source

class switch (cases:(((unit->bool)*source) list)) =
object (self)

  inherit operator (List.rev (List.map snd cases)) as super

  val orig_cases = cases
  val cases = List.rev cases

  method stype = Fallible

  method wake_up =
    List.iter (fun (b,s) -> s#get_ready (self:>operator)) cases ;

  method sleep =
    List.iter (fun (_,s) -> s#leave (self:>operator)) cases ;

  method authorized =
    List.fold_left
      (fun l (b,s) -> if b () then s::l else l) [] cases

  val mutable selected = None

  method is_ready = List.exists (fun s -> s#is_ready) self#authorized

  method get ab =
    assert (Mixer.Buffer.is_partial ab) ;
    let reselect () =
      selected <- None ;
      try
	List.iter
	  (fun s -> if s#is_ready then raise (Found s))
	  self#authorized
      with
	| Found s -> 
	    Log.logl ~label:"flow" 3 
	    (lazy (Log.f "%s switches to %s" id s#id)) ;
	    selected <- Some s
    in
      match selected with
	| None -> reselect () ; if selected <> None then self#get ab
	| Some s ->
	    s#get ab ;
	    if Mixer.Buffer.is_partial ab then
	      reselect ()

  method abort_track =
    match selected with
      | Some s -> s#abort_track
      | None -> ()
	  
end

(** Switch by date. *)
class date_switch cases =
object

  val my_cases = cases

  inherit switch
    (List.map (fun (d,s) -> (fun () ->
			       let ans = Scheduling.satisfied d in
				 ans
			    ), s) cases)

  method stype =
    let infallible_interval = List.fold_left (
      fun i (d,s) ->
	if s#stype = Infallible
	then Scheduling.disjunction i (Scheduling.compile d)
	else i
    ) (Scheduling.compiled_never ()) my_cases
    in
      if Scheduling.compiled_always () = infallible_interval
      then Infallible
      else Fallible

  method propagate_typing =
    List.fold_left
      (fun ok (d,s) ->
	 (s#add_activation_interval
	    (Scheduling.conjunction
	       (Scheduling.compile d) activation_intervals))
	 && ok )
      true my_cases

end

let _ =
  let proto = [ "arg_1",
		Lang.List_t (Lang.Product_t (Lang.String_t,
					     Lang.Source_t)),
		None,
		Some "Sources with the interval when they should be played"] in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "date_switch"
      ~doc:(Lang.to_doc
	      ("{date_switch [I1,S1;I2,S2;..]} "^
	       "At the beginning of a track, selects the first source Si "^
	       "such than the temporal predicate Ii is true.")
	      proto)
      ( fun p ->
	  let p = Lang.check cproto p in
	  let opts =
	    List.map (fun p ->
			match p with
			  | Lang.Product (i,s) ->
			      Scheduling.from_string (Lang.to_string i),
			      Lang.to_source s
			  | _ -> assert false)
	      (Lang.to_list (Hashtbl.find p "arg_1")) in
	    ((new date_switch opts):>source) )

(** Fallback selector. *)
class fallback childs =
object
  inherit switch (List.map (fun s -> (fun () -> true),s) childs)

  method stype =
    if List.exists (fun s -> s#stype = Infallible) childs
    then Infallible
    else Fallible

end

let _ =
  let proto = ["arg_1",
	       Lang.List_t Lang.Source_t,
	       None,
	       Some "I will select the first ready source in this list"] in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "fallback"
      ~doc:(Lang.to_doc "Selects its first ready child" proto)
      (fun p ->
	 let p = Lang.check cproto p in
	 let childs =
	   Lang.to_source_list (Hashtbl.find p "arg_1")
	 in
	   ((new fallback childs):>source))
