
open Types
open Play_files
open Dtools

class one_file_u uri =
object (self)
  inherit Play_files.unqueued

  val mutable req = None

  initializer
  let file = match self#create_request uri with
    | None -> assert false
    | Some r -> r
  in
    if Request.Resolved = Request.resolve file 60. then
      Request.set_metadata file "persistent" "true" 
    else
      raise (Lang.Invalid_value ("arg_1",
				 "Could not get a valid audio file")) ;
    req <- Some file

  method stype = Infallible
  method get_next_file = req
end

class one_file_q uri =
object (self)
  inherit Play_files.queued
  method get_next_request = self#create_request uri
end

let log n l = Log.logl ~label:"one_file" n (lazy l)

let _ =
  let proto =
    [ "arg_1", Lang.String_t, None,
      Some "URI where to find the file" ] in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register
      ~doc:(Lang.to_doc ("Loops on a request. It never fails if the request "^
			 "is static, meaning that it can be fetched once. "^
			 "Typically, http, ftp, say requests are static, "^
			 "and time is not.") proto)
      "one_file"
      ( fun p ->
	  let p = Lang.check cproto p in
	  let uri = Lang.to_string (Hashtbl.find p "arg_1") in
	    match Request.is_static uri with
	      | Some true ->
		  log 3 (Log.f "%S doesn't need a queue." uri) ;
		  ((new one_file_u uri) :> source)
	      | Some false ->
		  log 3 (Log.f "%S will be queued." uri) ;
		  ((new one_file_q uri) :> source)
	      | None -> raise (Lang.Invalid_value ("arg_1", "Invalid URI")) )
