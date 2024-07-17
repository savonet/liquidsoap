
open Types
open Lang_lexer

let from_in_channel safety_check liveness_check stdin =
  let scheduler =
    let lexbuf = Lexing.from_channel stdin in
    let print_error error =
      if Lexing.lexeme lexbuf = "" then
	Printf.printf "Line %d: %s\n" !line error
      else
	Printf.printf
	  "Line %d before %S: %s\n" !line (Lexing.lexeme lexbuf) error
    in
      try
	Lang_parser.scheduler Lang_lexer.token lexbuf
      with
	| Failure "lexing: empty token" -> print_error "Empty token" ; exit 1
	| Parsing.Parse_error -> print_error "Parse error" ; exit 1
	| Lang.Unbound s -> print_error
	    (Printf.sprintf
	       "Unbound symbol %S -- could be an unregistered operator" s) ;
	    exit 1
	| Lang.Missing_definition d ->
	    print_error
	    (Printf.sprintf "Missing parameter %S for %s" d !last_op) ;
	    exit 1
	| Lang.Wrong_label d ->
	    print_error
	    (Printf.sprintf "Unknown parameter %S for %s" d !last_op) ;
	    exit 1 
	| Lang.Wrong_type (d,s,t) ->
	    print_error
	    (Printf.sprintf
	       "Operator %s: %S has type %s but should have type %s"
	       !last_op d s (Lang.type_to_string t)) ;
	    exit 1
	| Lang.Invalid_value (d,s) ->
	    print_error
	    (Printf.sprintf
	       "Invalid parameter %S for operator %s: %s" d !last_op s) ;
	    exit 1
	| e -> print_error "Unknown error" ; raise e

  in
    if liveness_check && scheduler#stype = Fallible then
      Printf.eprintf "[WW] This scheduler is fallible !\n" ;

    scheduler#set_id "root" ;

    if safety_check &&
      not (scheduler#add_activation_interval (Scheduling.compiled_always ()))
    then
      Printf.eprintf "[WW] This scheduler may be unsafe.\n" ;

    scheduler

let from_file safety_check liveness_check filename =
  let ic = open_in filename in
  let sched = from_in_channel safety_check liveness_check ic in
    close_in ic ;
    sched
