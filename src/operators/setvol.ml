
open Types

class setvol (source:source) coeff =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method wake_up = source#get_ready (self:>operator)
  method sleep = source#leave (self:>operator)
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method get buf =
    let offset = Mixer.Buffer.already buf in
      source#get buf ;
      Mixer.Buffer.change_volume buf offset 
	((Mixer.Buffer.already buf)-offset) coeff

end

let _ = 
  let proto = [
    "arg_1", Lang.Float_t, None,
    (Some "multiplicative factor") ;
    "arg_2", Lang.Source_t, None, None ]
  in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "change_volume"
      ~doc:(Lang.to_doc "Changes the amplitude of the signal" proto)
      (fun p ->
	 let p = Lang.check cproto p in
	 let c = Lang.to_float (Hashtbl.find p "arg_1") in
	 let s = Lang.to_source (Hashtbl.find p "arg_2") in
	   ((new setvol s c):>source))
