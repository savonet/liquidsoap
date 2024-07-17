
open Types

class filter (source:source) freq q filter_type =
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
      Mixer.Buffer.simple_filter buf offset 
	((Mixer.Buffer.already buf) - offset) freq q filter_type

end

let _ = 
  let proto =
    [ "freq", Lang.Int_t, None, None ;
      "q", Lang.Float_t, None, None ;
      "mode", Lang.String_t, None, Some "low|high|band|notch" ;
      "arg_1", Lang.Source_t, None, None ]
  in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "filter"
      ~doc:(Lang.to_doc "performs some filtering of the signal" proto)
      (fun p ->
	 let p = Lang.check cproto p in
	 let freq, q, filter, src =
	   Lang.to_int (Hashtbl.find p "freq"),
	   Lang.to_float (Hashtbl.find p "q"),
	   Lang.to_string (Hashtbl.find p "mode"),
	   Lang.to_source (Hashtbl.find p "arg_1") in
	 let filter =
	   match filter with
	     | "low" -> Mixer.Low_pass
	     | "high" -> Mixer.High_pass
	     | "band" -> Mixer.Band_pass
	     | "notch" -> Mixer.Notch
	     | _ -> raise (Lang.Invalid_value
			     ("mode",
			      "valid values are low|high|band|notch"))
	 in
	   ((new filter src freq q filter):>source))
