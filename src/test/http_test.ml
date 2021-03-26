open Http

let mock_query = Buffer.create 10
let mock_response = ref ""

module T : Transport_t = struct
  type connection = string * int * int ref

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  let default_port = 999

  let connect ?bind_address:_ _ _ =
    (!mock_response, String.length !mock_response, ref 0)

  let wait_for ?log:_ _ _ = ()

  let write _ b ofs len =
    Buffer.add_subbytes mock_query b ofs len;
    len

  let read (v, len, pos) b bofs blen =
    if !pos < len then (
      let n = min blen (len - !pos) in
      let ofs = !pos in
      pos := !pos + n;
      Bytes.blit_string v ofs b bofs n;
      n)
    else 0

  let disconnect _ = ()
end

module H = Http.Make (T)
open H

let () =
  (* Normal get response. *)
  Buffer.reset mock_query;
  mock_response :=
    "HTTP/1.1 200 OK\r\ncontent-length: 10\r\nfoo: bar\r\n\r\n1234567890";
  let uri = { H.host = "foo"; port = None; path = "/foo" } in
  let status, headers, body = full_request ~timeout:1. ~uri ~request:Get () in
  let q =
    Printf.sprintf "GET /foo HTTP/1.0\r\nHost: foo\r\nUser-Agent: %s\r\n\r\n"
      Configure.vendor
  in
  assert (Buffer.contents mock_query = q);
  assert (status = ("HTTP/1.1", 200, "OK"));
  assert (headers = [("content-length", "10"); ("foo", "bar")]);
  assert (body = "1234567890");
  (* truncated get response. *)
  Buffer.reset mock_query;
  mock_response :=
    "HTTP/1.1 200 OK\r\ncontent-length: 10\r\nfoo: bar\r\n\r\n123456789";
  let uri = { H.host = "foo"; port = None; path = "/foo" } in
  let status, headers, body = full_request ~timeout:1. ~uri ~request:Get () in
  let q =
    Printf.sprintf "GET /foo HTTP/1.0\r\nHost: foo\r\nUser-Agent: %s\r\n\r\n"
      Configure.vendor
  in
  assert (Buffer.contents mock_query = q);
  assert (status = ("HTTP/1.1", 200, "OK"));
  assert (headers = [("content-length", "10"); ("foo", "bar")]);
  assert (body = "123456789");
  (* longer get response. *)
  Buffer.reset mock_query;
  mock_response :=
    "HTTP/1.1 200 OK\r\ncontent-length: 10\r\nfoo: bar\r\n\r\n12345678901";
  let uri = { H.host = "foo"; port = None; path = "/foo" } in
  let status, headers, body = full_request ~timeout:1. ~uri ~request:Get () in
  let q =
    Printf.sprintf "GET /foo HTTP/1.0\r\nHost: foo\r\nUser-Agent: %s\r\n\r\n"
      Configure.vendor
  in
  assert (Buffer.contents mock_query = q);
  assert (status = ("HTTP/1.1", 200, "OK"));
  assert (headers = [("content-length", "10"); ("foo", "bar")]);
  assert (body = "12345678901");
  (* Chunked response. *)
  Buffer.reset mock_query;
  mock_response :=
    "HTTP/1.1 200 OK\r\n\
     transfer-encoding: chunked\r\n\
     foo: bar\r\n\
     \r\n\
     2\r\n\
     12\r\n\
     3\r\n\
     345\r\n\
     0\r\n";
  let uri = { H.host = "foo"; port = None; path = "/foo" } in
  let status, headers, body = full_request ~timeout:1. ~uri ~request:Get () in
  let q =
    Printf.sprintf "GET /foo HTTP/1.0\r\nHost: foo\r\nUser-Agent: %s\r\n\r\n"
      Configure.vendor
  in
  assert (Buffer.contents mock_query = q);
  assert (status = ("HTTP/1.1", 200, "OK"));
  assert (headers = [("transfer-encoding", "chunked"); ("foo", "bar")]);
  assert (body = "12345")
