let read_all stdout =
  let buf = Buffer.create 1024 in
  let b = Bytes.create 1024 in
  let rec f () =
    let n = input stdout b 0 1024 in
    if 0 < n then (
      Buffer.add_subbytes buf b 0 n;
      f ())
  in
  f ();
  Buffer.contents buf

let () =
  let json = read_all stdin in
  let cmd, args, expected =
    let to_string = function `String s -> s | _ -> assert false in
    let to_list = function `Tuple l -> l | _ -> assert false in
    match Liquidsoap_lang.Json.from_string json with
      | `Assoc l ->
          ( to_string (List.assoc "cmd" l),
            List.map to_string (to_list (List.assoc "args" l)),
            to_string (List.assoc "expected" l) )
      | _ -> assert false
  in
  let process = Filename.quote_command cmd args in
  let stdout = Unix.open_process_in process in
  let output = read_all stdout in
  ignore (Unix.close_process_in stdout);
  if not (Pcre.pmatch ~rex:(Pcre.regexp expected) output) then (
    Printf.eprintf
      {|Error running command: %s %s
- Expected output:
%s

- Command output:
%s
|}
      cmd
      (String.concat " " (List.map (Printf.sprintf "%S") args))
      expected output;
    exit 1)
