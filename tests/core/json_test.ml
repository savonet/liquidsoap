let () =
  let s = "AverageLevel\000\196\016\000\000" in
  assert (
    Liquidsoap_lang.Json.to_string (`String s)
    = "\"AverageLevel\\u0000\\uFFFD\\u0000\\u0000\"")

let json =
  [|
    "{\n\
     \"firstName\": \"John\",\n\
     \"lastName\": \"Smith\",\n\
     \"isAlive\": true,\n\
     \"age\": 25,\n\
     \"address\": {\n\
     \"streetAddress\": \"21 2nd Street\",\n\
     \"city\": \"New York\",\n\
     \"state\": \"NY\",\n\
     \"postalCode\": \"10021-3100\"\n\
     },\n\
     \"phoneNumbers\": [\n\
     {\n\
     \"type\": \"home\",\n\
     \"number\": \"212 555-1234\"\n\
     },\n\
     {\n\
     \"type\": \"office\",\n\
     \"number\": \"646 555-4567\"\n\
     }\n\
     ],\n\
     \"children\": [],\n\
     \"spouse\": null\n\
     }\n";
    "{ \"face\": \"😂\" }";
    "{ \"face\": \"\\uD83D\\uDE02\" }";
  |]

let () =
  for i = 0 to Array.length json - 1 do
    ignore (Json.from_string json.(i))
  done

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

exception Failed

type expectation = [ `Success | `Failure | `Unspecified ]

let string_of_expectation = function
  | `Success -> "success"
  | `Failure -> "failure"
  | `Unspecified -> "unspecified"

let () =
  let pwd = Filename.dirname Sys.argv.(0) in
  let samples = Filename.concat pwd "json" in
  let files = List.sort Stdlib.compare (Array.to_list (Sys.readdir samples)) in
  let tests =
    List.filter_map
      (fun f ->
        match Filename.extension f with
          | ".json" ->
              let expectation =
                match f.[0] with
                  | 'y' -> `Success
                  | 'n' -> `Failure
                  | 'i' -> `Unspecified
                  | _ -> assert false
              in
              Some (expectation, f)
          | _ -> None)
      files
  in
  let run_test ((expectation : expectation), file) =
    Printf.printf "Testing file %s with %s expectation... %!" file
      (string_of_expectation expectation);
    try
      let v = Json.from_string (read_file (Filename.concat samples file)) in
      List.iter
        (fun (compact, json5) -> ignore (Json.to_string ~compact ~json5 v))
        [(true, true); (true, false); (false, true); (false, false)];
      match expectation with
        | `Success -> Printf.printf "OK!\n%!"
        | `Failure -> raise Failed
        | `Unspecified -> Printf.printf "SUCCESS!\n%!"
    with
      | Failed ->
          Printf.printf "FAILED!\n%!";
          exit 1
      | _ when expectation = `Failure -> Printf.printf "OK!\n%!"
      | _ when expectation = `Unspecified -> Printf.printf "FAILED!\n%!"
  in
  List.iter run_test tests

let () =
  let pwd = Filename.dirname Sys.argv.(0) in
  let samples = Filename.concat pwd "json5" in
  let files = List.sort Stdlib.compare (Array.to_list (Sys.readdir samples)) in
  let tests =
    List.filter_map
      (fun f ->
        match Filename.extension f with
          | ".json" | ".json5" -> Some (`Success, f)
          | ".txt" | ".js" -> Some (`Failure, f)
          | _ -> None)
      files
  in
  let run_test ((expectation : expectation), file) =
    Printf.printf "Testing file %s with %s expectation... %!" file
      (string_of_expectation expectation);
    try
      let v =
        Json.from_string ~json5:true (read_file (Filename.concat samples file))
      in
      List.iter
        (fun compact -> ignore (Json.to_string ~compact ~json5:true v))
        [true; false];
      if expectation <> `Success then raise Failed;
      Printf.printf "OK!\n%!"
    with
      | Failed ->
          Printf.printf "FAILED!\n%!";
          exit 1
      | _ when expectation = `Failure -> Printf.printf "OK!\n%!"
  in
  List.iter run_test tests

let () =
  let pwd = Filename.dirname Sys.argv.(0) in
  let basedir = Filename.concat pwd "big-list-of-naughty-strings" in
  let strings_file = Filename.concat basedir "blns.json" in
  Printf.printf "Testing big-list-of-naughty-strings...\n%!";
  let s = read_file strings_file in
  let v = Json.from_string s in
  ignore (Json.to_string ~compact:true v);
  ignore (Json.to_string ~compact:false v)
