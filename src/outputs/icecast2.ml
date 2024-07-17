
(** Output to an icecast server. *)

open Dtools
open Shout
open Vorbis

let output name loop =

  (*<section icecast2>*)

  let log = Log.log ~label:name in
  let logl = Log.logl ~label:name in

  let host = Conf.get_string ~root:name ~default:"localhost" "host" in
  let port = Conf.get_int ~root:name ~default:8000 "port" in
  let user = Conf.get_string ~root:name ~default:"source" "user" in
  let password = Conf.get_string ~root:name ~default:"hackme" "password" in
  let genre = Conf.get_string ~root:name ~default:"Misc" "genre" in
  let url =
    Conf.get_string ~root:name ~default:"http://savonet.sf.net" "url" in
  let description =
    Conf.get_string ~root:name ~default:"OCaml Radio !" "description" in
  let mount = Conf.get_string ~root:name ~default:name "mount" in
  let public = Conf.get_bool ~root:name ~default:false "public" in
  let bitrate = Conf.get_int ~root:name ~default:128 "bitrate" in
  let stereo = Conf.get_bool ~root:name ~default:true "stereo" in
  let freq = Conf.get_int ~root:name ~default:44100 "sample_freq" in

  (* For RTP, a very efficient protocol. *)
  let multicast_ip =
    Conf.get_string ~root:name ~default:"no_multicast" "multicast_ip" in

  (*</section icecast2>*)

  let sync,send,delay,close =
    let conn = new_shout () in

      logl 3 (lazy (Log.f
                      "Connecting mount %s for %s@%s" 
                      mount user host)) ;

      set_host conn host ;
      set_port conn port ;
      set_user conn user ;
      set_password conn password ;

      set_genre conn genre ;
      set_url conn url ;
      set_description conn description ;

      set_name conn name ;
      set_mount conn mount ;

      set_format conn Shout.Format_vorbis ;
      set_protocol conn Shout.Protocol_http ;
      set_public conn public ;

      if multicast_ip <> "no_multicast" then
        set_multicast_ip conn multicast_ip ;

      begin
	try
	  open_shout conn ;
	with
	  | No_connect ->
          failwith
            (Printf.sprintf "unable to connect icecast server %S !" host)
	  | No_login ->
	      failwith "icecast login failed : mount point already taken ?"
      end ;

      log 3 "Connection setup was successful." ;

      ( (fun () -> Shout.sync conn) ,
	(fun buf -> Shout.send conn buf) ,
	(fun () -> Shout.delay conn) ,
	(fun () -> Shout.close conn) )

  in

  let encoder,first_header =
    log 3 "Setting up an Ogg/Vorbis encoder..." ;
    Vorbis.create_encoder
      (Oggfile.get_ogg_enc_params ~stereo:stereo ~freq:freq bitrate)
      ~title:"Liquidsoap" ~artist:"The Savonet Team"
  in
  let encode = Vorbis.encode_buffer encoder in
  let reset = Oggfile.reset encoder in

    log 3 "Sending the first header ..." ;
    send first_header ;

    loop
      (fun wav ->
         match Mixer.Buffer.get_metadata wav with
           | None ->
               log 4 "Encoding..." ;
               let ogg = encode (Mixer.Buffer.to_string wav) in
                 log 4 "Sync..." ;
                 sync () ;
                 log 4 "Sending..." ;
                 send ogg ;
                 log 4 "Done."
           | Some m ->
               log 4 "Encoding (reset)..." ;
               let h = reset m in
               let ogg = encode (Mixer.Buffer.to_string wav) in
                 log 4 "Sync..." ;
                 sync () ;
                 log 4 "Sending..." ;
                 send h ; send ogg ;
                          log 4 "Done." ) ;

    close ()

let _ =
  Output.plug#register "icecast2" output ;
  Output.plug#register
    ~doc:(Doc.trivial
            "Shortcut for icecast2, since old icecast support is not included.")
    "icecast" output
