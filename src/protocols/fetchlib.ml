
(** Protocol plugin using ocaml-fetch *)

open Dtools

let log n l = Log.logl ~label:"fetch" n (lazy l)

external core_exit : int -> 'a = "unix_exit"

let time = Unix.gettimeofday

let fetch proto arg req maxtime =
  if String.length arg > 4 then
    let ext = String.sub arg ((String.length arg)-4) 4 in
    let local = Filename.temp_file "fetch" (String.lowercase ext) in
    let url = proto ^ ":" ^ arg in
    let t0 = time () in
    let ret =
      let pid_dl = flush_all () ; Unix.fork () in
	if pid_dl = 0 then
	  begin
	    try
	      Fetch.cp url ("file://" ^ local) ;
	      core_exit 0
	    with
	      | e ->
		  Printf.printf "Download failed: %S for %S \n%!"
		  (Printexc.to_string e) url ;
		  core_exit 1 ;
	  end
	else
	  let rr = ref None in
	    log 4 (Log.f "Downloading %S to %S (pid=%d)"
		     url local pid_dl);
	    while !rr = None do
	      if time () > maxtime then
		( Unix.kill pid_dl Sys.sigkill ;
		  ignore (Unix.waitpid [] pid_dl) ;
		  rr := Some (Unix.WEXITED 2) )
	      else
		let p,r = Unix.waitpid [Unix.WNOHANG] pid_dl in
		  if p <> 0 then rr := Some r else Unix.sleep 1
	    done ;
	    match !rr with
	      | None -> assert false
	      | Some r -> r
    in
    let t1 = time () -. t0 in
      match ret with
	| Unix.WEXITED 0 ->
	    log 4 (Log.f "Downloaded %S to %S in %f sec." url local t1) ; 
	    Request.push_indicator req [local] ;
	    Request.set_metadata req "temporary" "true"
	| Unix.WEXITED 2 ->
	    log 4 (Log.f
		     "Timeout (%f sec.) while downloading %S to %S"
		     t1 url local) ;
	    ( try Unix.unlink local with _ -> () ) ;
	    Request.log req (Printf.sprintf
			       "Timeout for %S !" url)
	| Unix.WEXITED 1 ->
	    log 4 (Log.f "Downloading of %S to %S failed in %f sec."
		     url local t1) ;
	    ( try Unix.unlink local with _ -> () ) ;
	    Request.log req (Printf.sprintf
			       "Failed to download %S !" url)
	| _ ->
	    log 4 (Log.f "Unknown error while downloading %S to %S (%f sec.)"
		     url local t1) ;
	    ( try Unix.unlink local with _ -> () ) ;
	    Request.log req (Printf.sprintf
			       "Failed to download %S !" url)
  else
    Request.log req ("Invalid URL " ^ proto ^ ":" ^ arg)

let _ =
  List.iter (fun p -> Request.protocols#register p 
	       { Request.resolve = fetch p ;
		 Request.static = true } ) ["ftp"; "smb"]
