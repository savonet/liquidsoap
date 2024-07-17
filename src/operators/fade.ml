
open Types

let length = 10

class fade source =
object (self)

  inherit operator [source] as super

  method stype = source#stype

  method wake_up = source#get_ready (self:>operator) ;
  method sleep = source#leave (self:>operator) ;

  val mutable count = -1

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method get ab =
    let offset1 = Mixer.Buffer.already ab in
      source#get ab ;
      count <- count+1 ;
      if Mixer.Buffer.is_partial ab 
      then 
	( count <- -1 ;
	  Printf.fprintf stdout "[II] Fade : end of the track.\n" ; )
      else
	let offset2 = Mixer.Buffer.already ab in
	let change_volume n =
	  if n<=0 || n>=length then () else
	    ( Printf.printf "[II] Fade %d\n" n ;
	      Mixer.Buffer.change_volume ab offset1 (offset2-offset1) 
		((float n)/.(float length)) )
	in
	  change_volume count ;
	  change_volume source#remaining

end

let _ =
  let proto = ["arg_1",
	       Lang.Source_t,
	       None,
	       None] in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "fade"
      (fun p ->
	 let s = Lang.to_source
		   (Hashtbl.find (Lang.check cproto p) "arg_1") in
	   ((new fade s):>source))
