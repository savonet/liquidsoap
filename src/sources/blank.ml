
open Types

class blank =
object
  inherit source
  method stype = Infallible
  method sleep = ()
  method wake_up = ()
  method is_ready = true
  val mutable must_fail = false
  method abort_track = must_fail <- true
  method get ab =
    if must_fail then must_fail <- false else
      ( Mixer.Buffer.blankify ab ;
	Mixer.Buffer.set_already ab Mixer.Buffer.size )
end

let _ =
  let proto = Lang.internal_of_prototype [] in
    Lang.operators#register
      ~doc:(Lang.to_doc "This source is not very noisy :)" [])
      "blank"
      (fun p -> let _ = Lang.check proto p in
		 ((new blank):>source))
