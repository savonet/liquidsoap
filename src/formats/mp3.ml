
(** Decode and read metadatas of mp3 files. *)

open Dtools

let decoder file =

  let info = Mp3info.InfoLocal.read_info file in
  let format = {
    Mixer.channels = Mad.wav_output_channels ;
    Mixer.sample_freq = info.Mp3info.samplefreq ;
    Mixer.sample_size = Mad.wav_output_sample_size ;
    Mixer.big_endian = Mad.wav_output_big_endian ;
    Mixer.signed = Mad.wav_output_signed ;
  } in
  let fd = 
    Log.logl ~label:"mp3" 3 (lazy (Log.f "open %S" file)) ; 
    Mad.openfile file
  in
  let abg = Mixer.Generator.create () in
  let stats = Unix.stat file in
  let in_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_bytes = ref 0 in
  let remaining () =              
    (* Computes an approximative remaining number of wav frames. *)
    if !in_bytes = 0 || !out_bytes = 0
    then -1
    else
      max 0
	((in_size - !in_bytes) /
	 (!in_bytes / (
	    (!out_bytes+(Mixer.Generator.length abg))
	    /Mixer.Buffer.size
	  ))
	)
  in
  let closed = ref false in
  let close () = 
    if not !closed then (
      Log.logl ~label:"mp3" 3 (lazy (Log.f "close %S" file)) ; 
      closed := true ;
      Mad.close fd )
  in
  let fill = 
    fun buf ->

      begin
	try
	  while
	    Mixer.Generator.should_be_feeded abg
	  do
	    let frame = Mad.decode_frame fd in
	      Mixer.Generator.feed abg format frame ;
	  done
	with
	  | _ -> ()
      end ;

      let offset = Mixer.Buffer.already buf in
	Mixer.Buffer.fill buf abg ;
	let added = (Mixer.Buffer.already buf) - offset in
	  in_bytes := Mad.get_current_position fd ;
	  out_bytes := !out_bytes + added ;
	  if Mixer.Buffer.is_partial buf 
	  then ( close () ; 0 ) 
	  else (  let r = remaining () in
		    Log.logl ~label:"mp3" 4
		      (lazy (Log.f "remaining %d in %s" r file)) ;  
		    r )
  in
    { Decoder.fill = fill ; Decoder.close = close }

let _ = Decoder.formats#register "MP3"
	  (fun name -> try Some (decoder name) with _ -> None)

let metadatas file =
  try
    Mp3id3.Id3Local.read_tag file
  with
    | _ -> []

let _ = Request.mresolvers#register "MP3" metadatas
