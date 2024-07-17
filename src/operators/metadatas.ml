
open Types

(* Here we take care to not introduce new redexes when substituting *)
let forbidden = Str.regexp "\\(@\\|(.*)\\)"
let clean s =
  Str.global_replace forbidden "" s

(* @id(something) -> something if id is defined
 * @id -> its value if defined *)
let pattern = Str.regexp "@\\([^(@)]+\\)(\\([^()]*\\))"
let interpolation s m =
  let rec aux s =
    if
      try ignore (Str.search_forward pattern s 0) ; true
      with Not_found -> false
    then
      aux
 	(let start = Str.match_beginning () in
	 let mend = Str.match_end () in
	 let sub = Str.matched_string s in
	 let param = Str.matched_group 1 s in
	 let body = Str.matched_group 2 s in
	   (String.sub s 0 start) ^
	   ( if Hashtbl.mem m param then
	       Str.global_replace
		 (Str.regexp ("@"^param))
		 (clean (Hashtbl.find m param))
		 body
	     else "" ) ^
	   (String.sub s mend ((String.length s)-mend)) )
    else
      s
  in
    aux s

class metadatas source pattern =
object (self)
  inherit operator [source]

  method remaining = -1

  method stype = source#stype
  method wake_up = source#wake_up
  method sleep = source#sleep
  method is_ready = source#is_ready

  val mutable in_meta = false

  (* dec and req can be manipulated by different threads... *)
  val lock = Tutils.Mutex.create "Metadatas.lock"
  val mutable req = None
  val mutable dec = None

  method resolve r =
    let a = Request.resolve r 20. in
      (* We need to check that what we resolved is still needed... *)
      Mutex.lock lock ;
      if match req with
	| None -> false
	| Some req -> Request.get_id req = Request.get_id r
      then
        begin
        assert (dec = None) ;
	match a with
          | Request.Resolved ->
              dec <- Some
	               (match Decoder.get
                         (match Request.get_filename r with
                            | None -> assert false
                            | Some f -> f)
	                with
                         | None -> assert false
                         | Some d -> d) ;
	      Mutex.unlock lock
	  | _ -> Request.destroy r ; req <- None ; Mutex.unlock lock
        end
      else
	( Mutex.unlock lock ; Request.destroy r )

  method uri meta = interpolation pattern meta

  method get buf =
    if not in_meta then
      begin
        source#get buf ;
        if Mixer.Buffer.is_partial buf then
          (in_meta <- true ; self#get buf)
        else
          if req = None then
            let q = Mixer.Buffer.copy_metadatas buf in
            let meta = ref None in
              begin try
                while true do meta := Some (Queue.pop q) done
              with
                | Queue.Empty -> ()
              end ;
                match !meta with
                  | None -> ()
                  | Some m ->
		      match Request.create (self#uri m) with
			| Some r ->
			    Mutex.lock lock ;
			    req <- Some r ;
			    assert (dec = None) ;
			    Mutex.unlock lock ;
			    ignore (Tutils.create
				      self#resolve r
				      "Metadata request resolving")
			| None -> ( (* no available request id *) )
      end
    else
      match dec with
	| Some d ->
	    ignore (d.Decoder.fill buf) ;
	    if Mixer.Buffer.is_partial buf then
	      ( d.Decoder.close () ;
		dec <- None ;
		Request.destroy
		  (match req with Some r -> r | None -> assert false) ;
		req <- None ;
		in_meta <- false )
	| None -> self#reset_meta

  method reset_meta =
    in_meta <- false ;
    Mutex.lock lock ;
    match req with
      | Some r ->
	  ( match dec with
	      | Some d -> d.Decoder.close () ; dec <- None ; Request.destroy r
	      | None -> () ) ;
	  req <- None ;
	  Mutex.unlock lock
      | None -> assert (dec = None) ; Mutex.unlock lock

  method abort_track =
    self#reset_meta ;
    source#abort_track

end

let default_pattern =
  "say:lauren/@artist(It was @artist@title(, @title))"

let register =
  let proto =
    [ "arg_1", Lang.Source_t, None, None ;
      "pattern", Lang.String_t, Some (Lang.String default_pattern), None ]
  in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "say_metadatas"
      ~doc:(Lang.to_doc
	      "Pronounces the title/author/... after a track."
	      proto)
      (fun p ->
	 let p = Lang.check cproto p in
	 let source = Lang.to_source (Hashtbl.find p "arg_1") in
	 let pattern = Lang.to_string (Hashtbl.find p "pattern") in
	   ((new metadatas source pattern):>source)) ;
