
(** Random switching at every new track among sources *)

open Types
open Dtools

exception Found of source

class random weights childs =
object (self)

  inherit operator childs as super

  method stype =
    if List.exists (fun s -> s#stype = Infallible) childs
    then Infallible
    else Fallible

  method wake_up =
    List.iter (fun s -> s#get_ready (self:>operator)) childs ;

  method sleep =
    List.iter (fun s -> s#leave (self:>operator)) childs ;

  val mutable selected = None

  method is_ready = List.exists (fun s -> s#is_ready) childs

  method get ab =
    assert (Mixer.Buffer.is_partial ab) ;
    let reselect () =
      selected <- None ;
      let (ready_list,n) =
	List.fold_left2
	  (fun (l,k) s w ->
	     if s#is_ready then begin
	       (s,w)::l,(k+w) end
	     else l,k) 
	  ([],0) childs weights
      in
	try
	  let sel = Random.int n in
	    ignore (List.fold_left
		      (fun k (s,w) ->
			 if k+w > sel then raise (Found s) else (k+w))
		       0 ready_list)
	with
	  | Invalid_argument "Random.int" -> () (* Happens when n=0 *)
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

let list_1 n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (1::acc) (n-1)
  in aux [] n

let _ =
  let proto =
    [ "arg_1",
      Lang.List_t Lang.Source_t,
      None,
      None ]
  in
  let wproto =
    [ "arg_1",
      Lang.List_t (Lang.Product_t (Lang.Source_t, Lang.Int_t)),
      None,
      Some ("Given [ ... ; (s_i,w_i) ; ... ], the source s_i will "^
	    "be chosen with probability w_i/(sum w_j)") ] in
  let cproto = Lang.internal_of_prototype proto in
  let cwproto = Lang.internal_of_prototype wproto in

    Lang.operators#register "random"
      ~doc:(Lang.to_doc
	      "At the beginning of every track, selects a random ready child."
	      proto)
      (fun p ->
	 let childs = Lang.to_source_list
			(Hashtbl.find (Lang.check cproto p) "arg_1")
	 in
	 let opts = list_1 (List.length childs) in
	   ((new random opts childs):>source)) ;

    Lang.operators#register "random_w"
      ~doc:(Lang.to_doc
	      "Same as random, but choice is no more equiprobable"
	      wproto)
      (fun p ->
	 let childs = List.map
			(function
			   | Lang.Product (a,b) ->
			       Lang.to_source a,
			       Lang.to_int b
			   | _ -> assert false)
			(Lang.to_list
			   (Hashtbl.find
			      (Lang.check cwproto p) "arg_1"))
	 in
	   ((new random (List.map snd childs) (List.map fst childs)):>source))
