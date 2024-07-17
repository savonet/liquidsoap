
(** Generate a sine *)

open Types

class sine freq =
object
  inherit source

  method sleep = ()
  method wake_up = ()
  method is_ready = true

  val mutable must_fail = false
  method abort_track = must_fail <- true

  val mutable phi = 0.

  method get ab =
    if must_fail then must_fail <- false else
      let off = Mixer.Buffer.already ab in
	phi <- Mixer.Buffer.sine ab off (Mixer.Buffer.size - off) freq phi;
	Mixer.Buffer.set_already ab Mixer.Buffer.size

  method stype = Infallible
end

let _ =
  let proto =
    [ ("arg_1", Lang.Int_t, None, (Some "Frequency of the sine")) ]
  in
  let cproto = Lang.internal_of_prototype proto in
    Lang.operators#register "sine"
      ~doc:(Lang.to_doc "Plays a boring sine..." proto)
      ( fun p ->
	  let p = Lang.check cproto p in
	    (new sine (Lang.to_int (Hashtbl.find p "arg_1")) :> source) )
