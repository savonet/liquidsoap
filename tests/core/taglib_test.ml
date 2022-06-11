let int_tags = [("year", Taglib.tag_year); ("tracknumber", Taglib.tag_track)]
let tag_aliases = [("track", "tracknumber")]

let () =
  let fname = if Array.length Sys.argv > 1 then Sys.argv.(1) else "test.mp3" in
  if not (Sys.file_exists fname) then (
    Printf.printf "File %s not found, exiting." fname;
    exit 0);
  Printf.printf "Reading %s...\n%!" fname;
  let f = Taglib.File.open_file `Autodetect fname in
  let tags =
    List.fold_left
      (fun cur (lbl, fn) ->
        try
          let v = fn f in
          if v = 0 then cur else (lbl, string_of_int v) :: cur
        with Not_found -> cur)
      [] int_tags
  in
  let tags =
    Hashtbl.fold
      (fun key (values : string list) tags ->
        let key = String.lowercase_ascii key in
        let key = try List.assoc key tag_aliases with _ -> key in
        if List.mem_assoc key tags || values = [] then tags
        else (
          let v = List.hd values in
          match v with
            | "0" when List.mem_assoc key int_tags -> tags
            | "" -> tags
            | _ -> (key, v) :: tags))
      (Taglib.File.properties f) tags
  in
  List.iter (fun (lbl, v) -> Printf.printf "%s: %s\n%!" lbl v) tags;
  Taglib.File.close_file f
