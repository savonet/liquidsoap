(** Show artist - title of a (list of) files. *)

let () =
  let fname = ref [] in
  Arg.parse [] (fun f -> fname := f :: !fname) "artist_title files";
  if !fname = [] then (
    Printf.eprintf "Please enter a filename.\n%!";
    exit 1);
  let fname =
    !fname
    |> List.map (fun f ->
           if String.contains f '*' then (
             let d = Filename.dirname f in
             let f =
               Filename.basename f
               |> Str.global_replace (Str.regexp "\\*") ".*"
               |> Str.regexp
             in
             let files =
               Sys.readdir d |> Array.to_list
               |> List.filter (fun s -> Str.string_match f s 0)
             in
             List.map (fun f -> d ^ "/" ^ f) files)
           else [f])
    |> List.flatten
  in
  List.iter
    (fun fname ->
      let m = Metadata.Any.parse_file fname in
      let artist = List.assoc_opt "artist" m |> Option.value ~default:"?" in
      let title = List.assoc_opt "title" m |> Option.value ~default:"?" in
      Printf.printf "%s - %s\n%!" artist title)
    fname
