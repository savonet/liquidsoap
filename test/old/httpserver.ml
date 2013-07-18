(* Simple webserver for testing... *)

let port = 8080

let file () =
  let buflen = 1024 in
  let buf = String.create buflen in
  let ans = ref "" in
  let ic = open_in "lame.html" in
  try
    while true do
      let n = input ic buf 0 buflen in
      if n = 0 then raise End_of_file;
      ans := !ans ^ String.sub buf 0 n
    done;
    assert false
  with
  | End_of_file -> close_in ic; !ans

let handle s =
  let buflen = 1024 in
  let buf = String.create buflen in
  let headers =
    let n = Unix.read s buf 0 buflen in
    String.sub buf 0 n
  in
  Printf.printf "*** Headers ***\n%s\n\n%!" headers;
  let f = file () in
  Printf.printf "Read file\n%!";
  let ans = Printf.sprintf "HTTP/1.1 200 OK\r\nDate: Mon, 23 May 2005 22:38:34 GMT\r\nServer: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)\r\nLast-Modified: Wed, 08 Jan 2003 23:11:55 GMT\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length f) f in
  Printf.printf "*** Answer ***\n%s\n\n" ans;
  Unix.write s ans 0 (String.length ans)

let () =
  Printf.printf "Creating server\n%!";
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, port) in
  let domain =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
    | Unix.ADDR_INET (_, _) -> Unix.PF_INET
  in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let handle_connexion (s,caller) =
    Printf.printf "Handle connection.\n%!";
    let inet_addr_of_sockaddr = function
      | Unix.ADDR_INET (n, _) -> n
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_any
    in
    let inet_addr = inet_addr_of_sockaddr caller in
    Printf.printf "Openning connection for %s.\n%!" (Unix.string_of_inet_addr inet_addr);
    handle s
  in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 100;
  Printf.printf "Listening for connections.\n%!";
  while true do
    let (s, caller) = Unix.accept sock in
    ignore (Thread.create handle_connexion (s, caller));
  done
