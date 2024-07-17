
open Types

let try_agent_locate l =
  try
    (Savonet_client.get_server_agent ())#locate l
  with
    | Savonet_client.Agent.Locate_Error i as e ->
	Printf.eprintf "[WW] locate failed: %s\n"
	begin match i with
	  | 0 -> "Error: channel master agent not found"
	  | 1 -> "Error: channel agent not found"
	  | 2 -> "Error: request agent not found"
	  | _ -> "ImpossibleError: module Request"
	end ;
	raise e

let try_agent_send a b =
  try
    (Savonet_client.get_kernel_agent ())#send a b
  with e ->
    Printf.eprintf "[WW] Send failed: %s\n"
    begin match e with
      | Savonet_client.Agent.Req_Error Savonet.Protocol.Req_Unjoinable ->
	  "Error: the channel request agent is unjoinable"
      | Savonet_client.Agent.Req_Error Savonet.Protocol.Req_Unauthorized ->
	  "Error: the channel request agent don't authorized us"
      | Savonet_client.Agent.Req_Error Savonet.Protocol.Req_Shunned ->
	  "Error: the channel request agent do not handle requests"
      |	e -> Printexc.to_string e
    end ;
    raise e

let already_created = ref false

class request_class agent_name =
object (self)
  inherit Play_files.queued as queued

  initializer
    if !already_created then
      failwith "There can be only one Savonet user-request agent !"
    else
      already_created := true

  val requests = Queue.create ()
  method get_next_request =
    try
      Some (Queue.take requests)
    with
      | Queue.Empty -> None

  (** Connects to strider. *)
(*
  method ask_request : 
    request -> (int * (string list)) list = fun req ->
      Printf.eprintf "[AA] Locating strider->request ...\n" ; 
      flush stderr ;
      let request_channel_agent =
	try_agent_locate ["strider";"request"]   (* TODO: hard coding *)
      in
	Printf.eprintf "[AA] Asking request ...\n" ; 
	flush stderr ;
	let r : ((int * (string list)) list)
	  = try_agent_send request_channel_agent (req,"geek.ogg") 
	in
	  Printf.eprintf
	    "[AA] Request answer received : %d possibilities\n"
	    (List.length r) ;
	  Printf.eprintf
	    "     Head : %d and %s\n" 
	    (fst (List.hd r)) (List.hd (snd (List.hd r))) ;
	  r
	    
  method ask_simple_request : 
    simple_request -> (int * (string list)) list = fun req ->
      Printf.eprintf "[AA] Locating strider->request ...\n" ; flush stderr ;
      let request_channel_agent =
	try_agent_locate ["strider";"simple_request"]
          (* TODO: no hard coding *)
      in
	Printf.eprintf "[AA] Asking simple request... %S, %S\n"
	  (fst req) (snd req) ;
	flush stderr ;
	let r : ((int * (string list)) list)
	  = try_agent_send request_channel_agent req 
	in
	  Printf.eprintf "[AA] Request answer received : %d possibilities\n"
	    (List.length r) ;
	  if List.length r <> 0
	  then
	    Printf.eprintf "     Head : %d and %s\n" 
	      (fst (List.hd r)) (List.hd (snd (List.hd r))) ;
	  r


  (** Handle for this channel request agent. *)

  method do_request me agent (question:talk_to_request) =
    Printf.eprintf "[II] Parsing request.\n" ; flush stderr ;
    match question with

      | File path ->
	  Printf.eprintf "[II] Got a File.\n" ; flush stderr ;
	  ( try
	      let req = self#create_traceable_request path in
		Queue.add req requests ;
		Request.get_id req
	    with
	      | Request.RID_lack -> (-1) )
      | _ -> -1

  initializer
    ignore (new Savonet_client.Agent.local
	      ~parent:((get_kernel_agent ())#t)
	      ~handle:self#do_request
	      agent_name)
*)    
end

let _ =
  let proto =
    [ "name", Lang.String_t, Some (Lang.String "request"),
      Some "name of the agent" ]
  in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "request"
      ~doc:(Lang.to_doc "(no doc)" proto)
      ( function p ->
	  let name =
	    Lang.to_string
	      (Hashtbl.find (Lang.check cproto p) "name")
	  in
	    ((new request_class name) :> source ))
