let () =
  Datachannel.init_logger Datachannel.Log_debug;

  let pc = Datachannel.create_peer_connection () in

  Datachannel.set_state_change_callback pc (fun state ->
      let s =
        match state with
          | `New -> "New"
          | `Connecting -> "Connecting"
          | `Connected -> "Connected"
          | `Disconnected -> "Disconnected"
          | `Failed -> "Failed"
          | `Closed -> "Closed"
      in
      Printf.printf "State: %s\n%!" s);

  let local_description_ready = Atomic.make false in
  Datachannel.set_gathering_state_change_callback pc (fun state ->
      let s =
        match state with
          | `New -> "New"
          | `In_progress -> "InProgress"
          | `Complete -> "Complete"
      in
      Printf.printf "Gathering State: %s\n%!" s;
      if state = `Complete then Atomic.set local_description_ready true);

  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  let dest_addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000) in

  let tr =
    Datachannel.add_track pc
      {
        direction = Direction_recvonly;
        codec = Codec_h264;
        payload_type = 96;
        ssrc = 42;
        mid = Some "video";
        name = None;
        msid = None;
        track_id = None;
        profile = None;
      }
  in

  Datachannel.chain_rtcp_receiving_session tr;

  Datachannel.set_message_callback tr (fun data size ->
      let _ =
        Unix.sendto_substring sock
          (Bytes.unsafe_to_string data)
          0 size [] dest_addr
      in
      ());

  Datachannel.set_local_description pc;

  Printf.printf "Expect RTP video traffic on localhost:5000\n%!";

  (* Wait for gathering to complete *)
  while not (Atomic.get local_description_ready) do
    Unix.sleepf 0.1
  done;

  let sdp = Datachannel.get_local_description pc in
  let sdp_type = Datachannel.get_local_description_type pc in
  Printf.printf "{\"type\":\"%s\",\"sdp\":\"%s\"}\n%!" sdp_type
    (String.escaped sdp);

  Printf.printf "Please copy/paste the answer provided by the browser:\n%!";
  let answer = input_line stdin in

  (* Minimal JSON parsing for {"type":"...","sdp":"..."} *)
  let find_field json key =
    let pat = Printf.sprintf "\"%s\":\"" key in
    let start =
      String.length pat
      +
        try Str.search_forward (Str.regexp_string pat) json 0
        with Not_found -> failwith (Printf.sprintf "field %s not found" key)
    in
    let end_ =
      try Str.search_forward (Str.regexp_string "\"") json start
      with Not_found -> failwith (Printf.sprintf "field %s unterminated" key)
    in
    String.sub json start (end_ - start)
  in
  let remote_type = find_field answer "type" in
  let remote_sdp = Scanf.unescaped (find_field answer "sdp") in

  Printf.printf "Got answer\n%!";
  Datachannel.set_remote_description pc ~sdp:remote_sdp ~sdp_type:remote_type;

  Printf.printf "Press Enter to exit.\n%!";
  let _ = input_line stdin in

  Datachannel.delete_peer_connection pc;
  Datachannel.cleanup ()
