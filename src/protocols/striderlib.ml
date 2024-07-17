
open Dtools
open Savonet

let log n l = Log.logl ~label:"strider" n (lazy l)

let _fetch arg req timeout =
  (* TODO: no hard coding *)
  let r = String2req.string2req arg in
  let request_channel_agent =
    (Savonet.get_server_agent ())#locate  ["strider";"request"]
  in
    log 3 (Printf.sprintf "Asking request %S." arg) ;
    let r : ((int * (string list)) list)
      = (Savonet.get_kernel_agent ())#send request_channel_agent r
    in
      log 3 (Printf.sprintf "Request answer received : %d possibilities.\n"
               (List.length r)) ;
      Request.push_indicator req
	(List.concat (List.map snd r))

let fetch arg req timeout =
  try _fetch arg req timeout with e ->
    log 2 (Printf.sprintf "Error while processing 'strider://%s' : %S."
	       arg (Printexc.to_string e)) ;
    Request.log req "Error while processing Strider request."

let _ =
  Request.register_protocol "strider"
    { Request.resolve = fetch ;
      Request.static = false }
