
open Dtools
open Shout
open Vorbis

let int_to_string n =
  let b0,b1,b2,b3 =
    n land 0xff, (n lsr 8) land 0xff,
    (n lsr 16) land 0xff, (n lsr 24) land 0xff in
  let ret = String.create 4 in
    ret.[0] <- char_of_int b0;
    ret.[1] <- char_of_int b1;
    ret.[2] <- char_of_int b2;
    ret.[3] <- char_of_int b3;
    ret

let string_to_int s =
  let b0,b1,b2,b3 =
    int_of_char s.[0],
    int_of_char s.[1],
    int_of_char s.[2],
    int_of_char s.[3] in
    b0 lor (b1 lsl 8)lor (b2 lsl 16) lor (b3 lsl 24)

let marsh_mutex = Mutex.create()
let marsh_tostring foo =
  let s = (Mutex.lock marsh_mutex; Marshal.to_string foo) in
    Mutex.unlock marsh_mutex; s

let output name =
  let fd_r, fd_w = Unix.pipe () in
  let ack_r, ack_w = Unix.pipe () in
  let son = flush_all (); Unix.fork () in
    if son <> 0 then begin
      let log = Log.log ~label:name in
      let logl = Log.logl ~label:name in
      let acks = String.create 1 in
	Unix.close ack_w; Unix.close fd_r;
	Root.output_loop
	  (fun () ->
	     let n = Unix.read ack_r acks 0 1 in
	       if n <> 1 then begin
		 log 1 "Encoder seems to be dead, exiting";
		 Unix.close ack_r; Unix.close fd_w;
		 ignore(Unix.waitpid [] son);
		 failwith "Encoder died"
	       end else begin
		 let marshaled = marsh_tostring
		   (Root.new_metadata (),
		    Mixer.Buffer.to_string Root.wav) [] in
		 let marsh_length = String.length marshaled in
		 let w1 = Unix.write fd_w (int_to_string marsh_length) 0 4 in
		   if w1 <> 4 then
		     failwith "1"
		   else
		     let w2 = Unix.write fd_w marshaled 0 marsh_length in
		       if w2 <> marsh_length then failwith "2"

	       end
	  );
	Unix.close ack_r; Unix.close fd_w;
	ignore(Unix.waitpid [] son)
    end else begin
      (*<section icecast2_fork>*)

      let host = Conf.get_string ~root:name ~default:"localhost" "host" in
      let port = Conf.get_int ~root:name ~default:8000 "port" in
      let user = Conf.get_string ~root:name ~default:"source" "user" in
      let password = Conf.get_string ~root:name ~default:"hackme" "password" in
      let genre = Conf.get_string ~root:name ~default:"Misc" "genre" in
      let url =
	Conf.get_string ~root:name ~default:"http://savonet.sf.net" "url" in
      let description =
	Conf.get_string ~root:name ~default:"OCaml Radio !" "description" in

      let name = Conf.get_string ~root:name ~default:name "name" in
      let mount = Conf.get_string ~root:name ~default:name "mount" in
      let public = Conf.get_bool ~root:name ~default:false "public" in
      let bitrate = Conf.get_int ~root:name ~default:128 "bitrate" in

      (* For RTP, a very efficient protocol. *)
      let multicast_ip =
	Conf.get_string ~root:name ~default:"no_multicast" "multicast_ip" in

      (*</section icecast2_fork>*)

      let sync,send,delay,close =
	let conn = new_shout () in
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
	      | No_connect -> failwith "unable to connect icecast server !"
	      | No_login ->
		  failwith "icecast login failed : mount point already taken ?"
	  end ;
	  ( (fun () -> Shout.sync conn) ,
	    (fun buf -> Shout.send conn buf) ,
	    (fun () -> Shout.delay conn) ,
	    (fun () -> Shout.close conn) )

      in

      let encoder,first_header =
	Vorbis.create_encoder
	  (Mixer.get_ogg_enc_params bitrate)
	  ~title:"Liquidsoap" ~artist:"The Savonet Team"
      in
      let encode = Vorbis.encode_buffer encoder in
      let reset = Oggfile.reset encoder in
      let sizebuf = String.create 4 in
	Unix.close ack_r; Unix.close fd_w;
	send first_header ;
	try
	  while true do
	    let r = Unix.write ack_w "\000" 0 1 in
	      assert(r = 1);
	      if Unix.read fd_r sizebuf 0 4 <> 4 then
		failwith "3"
	      else
		let marsh_size = string_to_int sizebuf in
		let buf = String.create marsh_size in
		let pos = ref 0 in
		  while !pos < marsh_size do
		    pos := !pos + Unix.read fd_r buf !pos (marsh_size
							   - !pos);
		  done;
	    let (metadata: (Request.metadata option)), (buffer: string) =
	      Marshal.from_string buf 0
	    in
	      match metadata with
		| None ->
		    let ogg = encode buffer in
		      sync ();
		      send ogg
		| Some m ->
		    let h = reset m in
		    let ogg = encode buffer in
		      sync ();
		      send h; send ogg
	  done
	with e ->
	  Unix.close ack_w; Unix.close fd_r; close (); raise e
    end

let _ =
  Root.outputs#register "icecast2_fork" output

