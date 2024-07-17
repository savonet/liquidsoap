
(** Output as a RTP stream. *)

(*<section rtp output>*)

let output name loop =
  (* Where to send the RTP stream *)
  let ip = Dtools.Conf.get_string ~root:name ~default:"224.0.1.20" "address" in
  let port = Dtools.Conf.get_int ~root:name ~default:8888 "port" in
  let session = Rtp.new_session Rtp.Send ip port in
    loop (fun w -> Rtp.send session w)

let _ = Output.plug#register "rtp" output
