(* A chunked connection whose server went away must end up disconnected:
   closing it writes a last chunk, which fails on a dead socket. *)

let server_alive = ref true

let socket =
  object
    method typ = "fake"
    method transport = Cry.unix_transport
    method file_descr = Unix.stdin
    method wait_for ?log:_ _ _ = ()

    method write _ _ len =
      if !server_alive then len
      else raise (Unix.Unix_error (Unix.EPIPE, "write", ""))

    method read buf _ _ =
      let answer = "HTTP/1.1 100 Continue\r\n\r\n" in
      Bytes.blit_string answer 0 buf 0 (String.length answer);
      String.length answer

    method close = ()
  end

let transport =
  object
    method name = "fake"
    method protocol = "http"
    method default_port = 8000
    method connect ?bind_address:_ ?timeout:_ ?prefer:_ _ _ = socket
  end

let () =
  let cry = Cry.create ~transport () in
  let connection =
    Cry.connection ~chunked:true ~protocol:(Cry.Http Cry.Put)
      ~mount:(Cry.Icecast_mount "/test") ~content_type:Cry.mpeg ()
  in
  Cry.connect cry connection;
  assert (Cry.get_status cry <> Cry.Disconnected);
  server_alive := false;
  (match Cry.close cry with
    | () -> assert false
    | exception Cry.Error (Cry.Write _) -> ());
  assert (Cry.get_status cry = Cry.Disconnected);
  server_alive := true;
  Cry.connect cry connection
