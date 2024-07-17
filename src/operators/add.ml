
(** Play multiple sources at the same time, and perform weighted mix *)

open Types

let max a b = if b = -1 || a = -1 then -1 else max a b
let min a b = if b = -1 then a else if a = -1 then b else min a b

class add (sources: (int*source) list) =
object (self)
  inherit operator (List.map snd sources) as super

  method stype =
    if List.exists (fun (_,s) -> s#stype = Infallible) sources
    then Infallible
    else Fallible

  method remaining = -1

  val debug =
    try 
      ignore (Sys.getenv "LIQUIDSOAP_DEBUG_ADD") ; true
    with 
      | Not_found -> false

  method wake_up =
    List.iter (fun (_,s) ->
		 s#get_ready (self:>operator) ) sources

  method sleep =
    List.iter (fun (_,s) -> s#leave (self:>operator)) sources

  val tmp = Mixer.Buffer.create ()

  method is_ready = List.exists (fun (_,s) -> s#is_ready) sources

  method get buf =
    assert (Mixer.Buffer.is_partial buf) ;
    let offset = Mixer.Buffer.already buf in
      if debug then Printf.printf "[II] add< %6d\n" offset ;
      Mixer.Buffer.blankify buf ;
      let weight,sources =
	List.fold_left
	  (fun (t,l) (w,s) -> if s#is_ready then (w+t),((w,s)::l) else t,l)
	  (0,[]) sources in
      let c = 1./.(float weight) in
      let first = ref true in
      let end_offset = List.fold_left (
	fun end_offset (w,s) ->
	  Mixer.Buffer.set_already tmp offset ; (* useless for first source *)
	  let c = (float w)*.c in
	  let base = ref offset in
	  let failure = ref 0 in
	  let buffer = if !first then buf else tmp in
	    if debug then
	      Printf.printf "[II] add+ %6d %s (%d)\n" !base s#id
		(Mixer.Buffer.already buffer) ;

	    while Mixer.Buffer.is_partial buffer && not (!failure = 2) do
	      s#get buffer ;
	      begin
		try 
		  while not !first do
		    Mixer.Buffer.push_metadata buf 
		      (Mixer.Buffer.pop_metadata buffer)
		  done 
		with
		  | Mixer.Buffer.No_metadata -> ()
	      end ;
	      let more = Mixer.Buffer.already buffer in
		if (!base = more) then incr failure else failure := 0 ;
		base := more ;
		if debug then Printf.printf "[II] add  %6d\n" !base ;
	    done ;

	    Mixer.Buffer.change_volume buffer offset (!base-offset) c ;
	    if not !first then
	      Mixer.Buffer.add buf offset tmp offset (!base-offset)
	    else
	      Mixer.Buffer.set_already buf offset ;
	    first := false ;
	    max end_offset !base

      ) offset sources in
	Mixer.Buffer.set_already buf end_offset ;
	if debug then Printf.printf "[II] add> %6d\n" end_offset ;

  method abort_track = 
    List.iter (fun (_,s) -> s#abort_track) sources

end

let register_add =
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
	    "have amplitude w_i/(sum w_j) in the sum") ] in
  let cproto = Lang.internal_of_prototype proto in
  let cwproto = Lang.internal_of_prototype wproto in

    Lang.operators#register "add"
      ~doc:(Lang.to_doc
	      "Sums its children, with renormalization"
	      proto)
      (fun p ->
	 let p = Lang.check cproto p in
	 let sources = Lang.to_source_list (Hashtbl.find p "arg_1") in
	   ((new add (List.map (fun s -> (1,s)) sources)):>source)) ;

    Lang.operators#register "add_w"
      ~doc:(Lang.to_doc
	      "Sums its children, with customizable renormalization"
	      wproto)
      (fun p ->
	 let p = Lang.check cwproto p in
	 let sources =
	   List.map
	     (function
		| Lang.Product (a,b) ->
		    Lang.to_int b, Lang.to_source a
		| _ -> assert false)
	     (Lang.to_list (Hashtbl.find p "arg_1"))
	 in
	   ((new add sources):>source))
