
open Types
open Unix
open External_request
open Dtools

let ignore_sigpipe =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let log = Log.log ~label:"telnet"

(** Telnet commands are simple.
  * The most complicated one are of the form : <prefix><parameter> ...
  * We have a tiny hand-made parser for that. *)

let prefix_check_and_extract p =
  let plen = String.length p in
  let check s =
    String.length s >= plen && p = String.sub s 0 plen
  in
  let extract s =
    String.sub s plen ((String.length s)-plen)
  in
    check, extract

let trace_req, trace_req_id = prefix_check_and_extract "trace "
let trace_req_id s = try int_of_string (trace_req_id s) with _ -> (-1)

let metadata_req, metadata_req_id = prefix_check_and_extract "metadatas "
let metadata_req_id s = try int_of_string (metadata_req_id s) with _ -> (-1)

let skip_req, skip_req_id = prefix_check_and_extract "skip "
let skip_req_id s = try int_of_string (skip_req_id s) with _ -> (-1)

let unskip_req, unskip_req_id = prefix_check_and_extract "unskip "
let unskip_req_id s = try int_of_string (unskip_req_id s) with _ -> (-1)

let request_req, request_string = prefix_check_and_extract "request "

(** [telnet_request] is an [external_request] which queue is feed by
  * requests made over the network using a simple telnet interface.
  * This interface also provides many services. *)
class telnet_request port =
object (self)
  inherit external_request as super

  method interaction i o =
    while true do
      output_string o "? " ; flush o ;
      let s = input_line i in
      let s = String.sub s 0 ((String.length s)-1) in
      let ans = match s with
(** This is only for debugging...
        | "locks" ->
            let l = Tutils.Mutex.get_list () in
              List.fold_left (fun s (k,v,l) ->
                                s^(Printf.sprintf
                                     "> %b : [%d] %s\n"
                                     v (Obj.magic l) k)) "" l
        | "threads" ->
            let l = Tutils.get_list () in
              List.fold_left (fun s (n,t) ->
                                s^(Printf.sprintf "> [%d] %s\n"
                                     (Obj.magic t) n)) "" l
*)
        | "uptime" ->
            let date = int_of_float (Root.uptime ()) in
              Printf.sprintf "> %dj %02dh %02dm %02ds"
                (date/(24*60*60))
                ((date mod (24*60*60)) / (60*60))
                ((date mod (60*60)) / 60)
                (date mod 60)
        | "skip" -> Root.skip () ; "> Skipped."
        | "alive" ->
            "> "^(String.concat " "
                    (List.map string_of_int (Request.alive_requests ())))
        | "on air" ->
            "> "^(String.concat " "
                    (List.map string_of_int (Request.on_air_requests ())))
        | "resolving" ->
            "> "^(String.concat " "
                    (List.map string_of_int (Request.resolving_requests ())))
        | "exit" -> failwith "This is a clean exiting."
        | "metadata"
        | "metadatas" ->
            let q = Root.get_metadatas () in
              "> Begin.\n"^
              (fst (Queue.fold
                      (fun (s,i) m ->
                         let s = s^"--- "^(string_of_int i)^" ---\n"^
                                 (Request.string_of_metadata m) in
                           s,(i-1))
                      ("",(Queue.length q)) q))^
              "> End."
        | s when skip_req s ->
            ( let id = skip_req_id s in
                match Request.from_id id with
                  | None -> "> No such request."
                  | Some r ->
                      Request.set_metadata r "skip" "true" ;
                      "> Will be skipped." )
        | s when unskip_req s ->
            ( let id = unskip_req_id s in
                match Request.from_id id with
                  | None -> "> No such request."
                  | Some r ->
                      Request.set_metadata r "skip" "false" ;
                      "> Won't be skipped." )
        | s when request_req s ->
            ( let req = request_string s in
                match self#create_request req with
                  | Some req ->
                      let id = Request.get_id req in
                        self#push_request req ;
                        ("> Your request has rid = "^(string_of_int id))
                  | None -> "> Unable to make a request !" )
        | s when trace_req s ->
            let id = trace_req_id s in
              begin
                match Request.from_id id with
                  | Some r ->
                      let log = Request.get_log r in
                      let log = Request.string_of_log log in
                        "> Begin.\n"^log^"> End."
                  | None -> "> No such request."
              end
        | s when metadata_req s ->
            let id = metadata_req_id s in
              begin
                match Request.from_id id with
                  | Some r ->
                      let m = Request.get_metadatas r in
                      let m = Request.string_of_metadata m in
                        "> Begin.\n"^m^"> End."
                  | None -> "> No such request."
              end
        | "queue" ->
            let q = self#copy_queue in
              "> "^
              (List.fold_left (fun s r -> (string_of_int (Request.get_id r))^
                                          " "^s) "" q)
        | _ ->
            "> Usage : 'exit' | 'on air' | 'resolving' | 'alive' | 'skip' "^
            "| 'queue' "^
            "| 'request <url>' | 'trace <rid>' "^
            "| 'metadatas' | 'metadatas <rid>'"
      in
        output_string o ans ; output_char o '\n' ; flush o
    done

  method wake_up =
    super#wake_up ;
    let handle s =
      try (* This try is not useless !!! *)
	let outchan = out_channel_of_descr s in
	let inchan = in_channel_of_descr s in
	  begin try
	    self#interaction inchan outchan
	  with e ->
	    Printf.eprintf "Telnet client abort: %s !\n%!"
	    (Printexc.to_string e)
	  end;
	  begin try
	    Unix.shutdown s Unix.SHUTDOWN_ALL
	  with
	    | Unix.Unix_error(Unix.ENOTCONN, "shutdown", "") -> ()
	  end;
	  Unix.close s;
	  Printf.eprintf "Telnet client: Close done.\n%!" ;
      with
	| e ->
	    Printf.eprintf "Exception outside interaction: %s !\n%!"
	    (Printexc.to_string e)
    in
    let sock = socket PF_INET SOCK_STREAM 0 in
      setsockopt sock SO_REUSEADDR true ;
      (try bind sock (ADDR_INET(inet_addr_any, port)) with
	 | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
	     raise (Lang.Invalid_value ("port", "already taken"))) ;
      listen sock 5 ;
      ignore
        ( Tutils.create
            ( fun () ->
                while true do
                  try
		    let (s,caller) = accept sock in
		    let ip =
                      let a = match caller with
                        | ADDR_INET (a,_) -> a
                        | _ -> assert false
                      in
                        try
                          (gethostbyaddr a).h_name
                        with
                          | Not_found -> string_of_inet_addr a
		    in
                      ignore (Tutils.create handle s
                                (Printf.sprintf "telnet request from %s" ip))
		  with e ->
		    Printf.eprintf
		    "Failed to launch a server instance: %s !\n%!"
		    (Printexc.to_string e)
		done ) ()
	    "telnet request main thread" )

end

let _ =
  let proto = [ "port", Lang.Int_t, Some (Lang.Int 1234), None ] in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "telnet"
      ~doc:(Lang.to_doc
              ("Launches a server that performs many commands. "^
               "Among them, clients can make some requests.")
              proto)
      ( fun p -> let p = Lang.check cproto p in
        let port = Lang.to_int (Hashtbl.find p "port") in
          ((new telnet_request port) :> source) )
