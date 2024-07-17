
module Encoding = Savonet.Encoding.Unsecure

module Access = Savonet.Access.Dummy
module User = Savonet.User.M
module Pack = Savonet.Pack.String
module Agent = Savonet.Agent.M (Pack) (Access) (User)

module Client = Savonet.Client.M (Agent)
module Group = Savonet.Group.Uncrypted

open Dtools

(** See examples/liquidsoap.conf for the params. *)

let log = Log.log ~label:"savonet"
let logl = Log.logl ~label:"savonet"

let server_agent = ref None
let kernel_agent = ref None

let get_server_agent () =
  match !server_agent with None -> failwith "No server agent !" | Some a -> a

let get_kernel_agent () =
  match !kernel_agent with None -> failwith "No kernel agent !" | Some a -> a

(*<section savonet>*)

let create_agents () =

  (* If this value is <n>, connection_key will be savonet.connect.<n> *)
  let connection_name = Conf.get_string "savonet.connect" in
  let connection_key = Conf.cons "savonet.connect" connection_name in
  let connection_type =
    (* Valid values are net_raw or net_ssl *)
    Conf.get_string ~root:connection_key ~default:"net_raw" "channel" in

  let connection_func =
    try
      List.assoc connection_type Configure.channels_types
    with
      | Not_found ->
	  failwith (Printf.sprintf "Unknown channel type %S !" connection_type)
  in
  let login = Conf.get_string ~root:connection_key "user.login" in
  let password = Conf.get_string ~root:connection_key "user.password" in

  let name = Conf.get_string ~root:connection_key "agent.name" in
  let port = Conf.get_int ~root:connection_key ~default:6800 "port" in
  let host =
    Conf.get_string ~root:connection_key ~default:"localhost" "host" in
    Conf.set_int (connection_key^".port") port ;
    Conf.set_string (connection_key^".host") host ;

    logl 3 (lazy (Log.f "Creating kernel agent %s for %s@%s:%d ..."
		    name
		    (Conf.get_string ~root:connection_key "user.login")
		    (Conf.get_string ~root:connection_key "host")
		    port )) ;

    let user = User.makelocal ~login ~password in
    let channel = connection_func connection_key in
    let client = Client.make ~channel ~user in
    let sa = Client.connect client in
      server_agent := Some sa ;
      let master_ca = match sa#find "channels" with
	| None -> failwith "No master channel agent found"
	| Some a -> a
      in
      let metadata me agent =
	logl 3 (lazy (Log.f "%S asked for metadatas called me\n" agent#name));
	Root.get_metadatas
      in
      let ka = new Agent.local ~parent:master_ca ~handle:metadata name in
      let trace me agent (id:int) =
	log 3 "Request trace wanted !" ;
	try
	  Request.get_log (Request.from_id id)
	with
	  | Not_found -> Queue.create ()
      in
	ignore (new Agent.local
		  ~parent:(ka#t)
		  ~handle:(fun a b s -> Root.skip ())
		  "skip") ;
	ignore (new Agent.local ~parent:ka#t ~handle:trace "trace") ;
	kernel_agent := Some ka ;
	log 3 "Kernel's agents created."

let _ =
  (* Means that savonet-related services have to be started *)
  let start = Conf.get_bool ~default:false "savonet" in
  Init.add_start [
    Init.make ~depends:[Log.start]
		    (fun () ->
		       if start then create_agents ())]

(*</section savonet>*)
