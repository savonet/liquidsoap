(* test name, (deps, args) *)
let test_params =
  [
    ( "json_test",
      ( "(:json ./json) (:json5 ./json5) (:big-list-of-naughty-strings \
         ./big-list-of-naughty-strings)",
        [""] ) );
    ( "stream_decoder_test",
      ( "(:test_wav ./test.wav) (:test_mp3 ./test.mp3)",
        ["%{test_wav} bla.wav"; "%{test_mp3} bla.wav"] ) );
    ("parsesrt", ("(:test_srt ./test.srt)", ["%{test_srt}"]));
  ]

(* Tests with custom rules that are not auto-generated *)
let skip_tests = ["ffmpeg_stream_description_test"; "ffmpeg_get_type_test"]
let test_names = ref []

let test_name s =
  let test_name = Filename.remove_extension s in
  test_names := Printf.sprintf "(alias %s)" test_name :: !test_names;
  test_name

let () =
  let location = Sys.getcwd () in
  let tests =
    List.sort Stdlib.compare
      (List.filter_map
         (fun f ->
           let name = Filename.remove_extension f in
           if
             f <> "gen_dune.ml"
             && Filename.extension f = ".ml"
             && not (List.mem name skip_tests)
           then Some name
           else None)
         (Build_tools.read_files ~location ""))
  in
  List.iter
    (fun test ->
      let deps, args =
        match List.assoc_opt test test_params with
          | None -> ("", [""])
          | Some (deps, args) -> (deps, args)
      in
      Printf.printf
        {|
(executable
 (name %s)
 (modules %s)
 (libraries liquidsoap_core liquidsoap_optionals))

(rule
 (alias %s)
 (package liquidsoap)
 (deps
  %s
  (:%s %s.exe))
 (action %s%s%s))

|}
        (test_name test) test test deps test test
        (if List.length args > 1 then "(progn " else "")
        (String.concat " "
           (List.map
              (fun arg -> Printf.sprintf "(run %%{%s} %s)" test arg)
              args))
        (if List.length args > 1 then ")" else ""))
    tests

let () =
  Printf.printf
    {|(alias
  (name citest)
  (deps
    %s))
|}
    (String.concat "\n    " (List.sort_uniq Stdlib.compare !test_names))
