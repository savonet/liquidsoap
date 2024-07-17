
(** Plug for file decoding, in which [src/formats] plugins come. *)

open Dtools

(** A decoder is a filling function and a closing function,
  * called as soon as a filling fails -- is not complete. *)
type decoder = {
  fill : Mixer.Buffer.t -> int ;
  close : unit -> unit ;
}

(** Plugins are given filenames and return a decoder, if possible. *)
let formats : (string -> decoder option) Plug.plug =
  Plug.create ~doc:"Method to read audio files." ~insensitive:true "formats"

(** Get a decoder for [filename]. *)
let get filename =
  if String.length filename < 3 then None else
    let ext = String.uppercase
		(String.sub filename ((String.length filename)-3) 3)
    in
      match formats#get ext with
	| Some decoder ->
	    Log.logl ~label:"decoder" 3 
	    (lazy (Log.f "New %s decoder for %S" ext filename)) ;
	    decoder filename
	| None -> None
