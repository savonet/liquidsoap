
(** Output in an Ogg/vorbis file. *)

open Dtools
open Vorbis

let get_ogg_enc_params ?stereo ?freq bitrate =
  {
    Vorbis.enc_min_bitrate = None;
    Vorbis.enc_max_bitrate = None;
    Vorbis.enc_quality = 0.5;
    Vorbis.enc_managed = false;

    Vorbis.enc_bitrate = Some bitrate;
    Vorbis.enc_channels = if stereo = Some false then 1 else 2 ;
    Vorbis.enc_sample_freq = if freq = None then Some 44100 else freq ;

    Vorbis.enc_in_channels = 2;
    Vorbis.enc_in_sample_freq = 44100;
    Vorbis.enc_in_sample_size = 16;
    Vorbis.enc_in_big_endian = false;
  }

let reset encoder m =
  let get h k =
    try
      Some (Hashtbl.find h k)
    with _ -> None
  in
  let getd h k d =
    try
      Some (Hashtbl.find h k)
    with _ -> Some d
  in
  let def_title =
    match get m "uri" with
      | Some s -> let title = Filename.basename s in
	  ( try
	      String.sub title 0 (String.rindex title '.')
	    with
	      | Not_found -> title )
      | None -> "Unknown"
  in
    Vorbis.encoder_reset_opt
      (getd m "title" def_title)
      (get m "artist")
      (get m "genre")
      (get m "date")
      (get m "album")
      (get m "tracknum")
      encoder

let output name loop =

  (*<section ogg output>*)
  (*<info Output in a file, using ogg/vorbis encoding >*)
  let fd = open_out
             (Conf.get_string ~root:name ~default:"/dev/null" "filename") in
  let bitrate = Conf.get_int ~root:name ~default:128 "bitrate" in
  let freq = Conf.get_int ~root:name ~default:44100 "sample_freq" in
  let stereo = Conf.get_bool ~root:name ~default:true "stereo" in

  (*</section ogg>*)
  let encoder,first_header =
    Vorbis.create_encoder
      (get_ogg_enc_params ~stereo:stereo ~freq:freq bitrate)
      ~title:"Liquidsoap" ~artist:"The Savonet Team"
  in
  let encode = Vorbis.encode_buffer encoder in
  let send = output_string fd in
  let reset = reset encoder in

    send first_header ;

    loop
      (fun wav ->
         match Mixer.Buffer.get_metadata wav with
           | None ->
               let ogg = encode (Mixer.Buffer.to_string wav) in
                 send ogg
           | Some m ->
               let h = reset m in
               let ogg = encode (Mixer.Buffer.to_string wav) in
                 send h ; send ogg ) ;

    close_out fd

let () =
  Output.plug#register "ogg" output
