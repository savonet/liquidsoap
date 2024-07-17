
(** Avoids some source to play a new track before some delay elapses after the
  * end of the previous track played. *)

open Types

class delay (source:source) delay =
object (self)
  inherit operator [source] as super

  method stype = Fallible

  method remaining = source#remaining
  method wake_up = source#get_ready (self:>operator)
  method sleep = source#leave (self:>operator)
  method abort_track = source#abort_track

  val mutable last = 0.
  val mutable failed = false

  method is_ready = (Unix.time ()) -. last >= delay

  method get buf =
    match failed,self#is_ready with
      | false, _ ->
	  source#get buf ;
	  if Mixer.Buffer.is_partial buf then
	    ( failed <- true ; last <- Unix.time () )
      | true, true ->
	  failed <- false ;
	  source#get buf
      | true, false -> ()

end

let _ = 
  let proto =
    [ "arg_1", Lang.Float_t, None,
      (Some ("The source won't be ready less than this amount of seconds "^
	     "after any end of track")) ;
      "arg_2", Lang.Source_t, None, None
    ]
  in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "delay"
      ~doc:(Lang.to_doc
	      ("Prevents the child from being ready again too fast after "^
	       "a end of track")
	      proto)
      (fun p ->
	 let p = Lang.check cproto p in
	 let d = Lang.to_float (Hashtbl.find p "arg_1") in
	 let s = Lang.to_source (Hashtbl.find p "arg_2") in
	   ((new delay s d):>source))
