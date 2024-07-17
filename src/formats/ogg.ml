
(** Decode and read metadatas of ogg/vorbis files. *)

let bytes_to_get = 1024*64

let decoder file = 
  let info = Vorbis.get_info file in
  let format = {
    Vorbis.channels = info.Vorbis.audio_channels ;
    Vorbis.sample_freq = Mixer.Buffer.format.Mixer.sample_freq ;
    Vorbis.sample_size = Mixer.Buffer.format.Mixer.sample_size ;
    Vorbis.big_endian = Mixer.Buffer.format.Mixer.big_endian ;
    Vorbis.signed = Mixer.Buffer.format.Mixer.signed ;
  } in
  let mixer_fmt = {      (* Same value, different type ... *)
    Mixer.channels = info.Vorbis.audio_channels ;
    Mixer.sample_freq = Mixer.Buffer.format.Mixer.sample_freq ;
    Mixer.sample_size = Mixer.Buffer.format.Mixer.sample_size ;
    Mixer.big_endian = Mixer.Buffer.format.Mixer.big_endian ;
    Mixer.signed = Mixer.Buffer.format.Mixer.signed ;
  } in
  let unix_fd = Unix.openfile file [Unix.O_RDONLY] 0 in
  let fd = Vorbis.open_dec_fd unix_fd format in
  let abg = Mixer.Generator.create () in
  let stats = Unix.stat file in
  let in_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_bytes = ref 0 in
  let remaining () = 
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
  let tmpbuf = String.create bytes_to_get in
  let close =
    let closed = ref false in fun () -> if not !closed then (
	closed := true ;
	Vorbis.close_dec_file fd )
  in
  let fill buf =
      
      begin
	try
	  while Mixer.Generator.should_be_feeded abg do
	    let l = Vorbis.decode fd tmpbuf 0 bytes_to_get 
	    in
	      in_bytes := Unix.lseek unix_fd 0 Unix.SEEK_CUR ;
	      Mixer.Generator.feed abg mixer_fmt
		(String.sub tmpbuf 0 l)
	  done ;
	with _ -> ()
      end ;

      let offset = Mixer.Buffer.already buf in
	Mixer.Buffer.fill buf abg ;
	let added = (Mixer.Buffer.already buf) - offset in
	  out_bytes := !out_bytes + added ;
	  if added = 0 then (close () ; 0) else remaining ()
  in
    { Decoder.fill = fill ; Decoder.close = close }

let _ = Decoder.formats#register "OGG"
	  (fun name -> try Some (decoder name) with _ -> None)

let metadatas file =
  try
    let (vs,comments) = Vorbis.get_comments file in
      Array.to_list comments
  with
    | _ -> []

let _ = Request.mresolvers#register "OGG" metadatas
