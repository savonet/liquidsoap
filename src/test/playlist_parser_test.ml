let finalize ~cleanup fn =
  try
    fn ();
    cleanup ()
  with exn ->
    cleanup ();
    raise exn

let () =
  let tmp = Filename.temp_file "foo" "bar" in
  let dir = Filename.dirname tmp in
  let base = Filename.basename tmp in
  let cleanup () = Sys.remove tmp in

  Playlist_parser.conf_uri_protocols#set
    ("bla" :: Playlist_parser.conf_uri_protocols#get);

  finalize ~cleanup (fun () ->
      (* pwd, input, expected *)
      let checks =
        [
          (None, tmp, tmp);
          (Some dir, base, tmp);
          (None, "annotate:foo=\"bar\":" ^ tmp, tmp);
          (Some dir, "annotate:foo=\"bar\":" ^ base, tmp);
          (None, "annotate:foo=\"bar\":http://foo", "http://foo");
          (Some "http://", "annotate:foo=\"bar\":foo", "http://foo");
          (None, "annotate:foo=\"bar\":bla:foo", "bla:foo");
          (Some "bla:", "annotate:foo=\"bar\":foo", "bla:foo");
          (Some "http://host/", "annotate:foo=\"bar\":" ^ tmp, tmp);
        ]
      in
      List.iter
        (fun (pwd, input, expected) ->
          let resolved = Playlist_parser.get_file ?pwd input in
          if resolved <> expected then
            failwith
              (Printf.sprintf "Input: pwd: %s, uri: %s, expected: %s, got: %s"
                 ( match pwd with
                   | None -> "None"
                   | Some p -> Printf.sprintf "Some %S" p )
                 input expected resolved))
        checks)
