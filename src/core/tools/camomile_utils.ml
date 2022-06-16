let camomile_dir = Liquidsoap_paths.camomile_dir

module C = CamomileLibrary.CharEncoding.Configure (struct
  let datadir = Filename.concat camomile_dir "database"
  let localedir = Filename.concat camomile_dir "locales"
  let charmapdir = Filename.concat camomile_dir "charmaps"
  let unimapdir = Filename.concat camomile_dir "mappings"
end)

exception Unknown_encoding of string

let enc_of_name s =
  try C.of_name s with Not_found -> raise (Unknown_encoding s)

let conf_tag =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "tag")
    "Settings related to metadata tags"

let conf_encoding =
  (* The ["foo";"bla"] may be stripped by configure here.. We avoided it by chance because
   * the configure script takes conf_tag#plug as the beginning of a comment.
   * Don't forget this when modifying this inclusion later... *)
  Dtools.Conf.list
    ~p:(conf_tag#plug "encodings")
    ~d:["UTF-8"; "ISO-8859-1"]
    "List of encodings to try for automatic encoding detection"

let custom_encoding = ref None

let get_encoding () =
  match !custom_encoding with
    | Some e -> e
    | None ->
        let encs = conf_encoding#get in
        let e = C.automatic "LIQ-TAGS" (List.map enc_of_name encs) C.utf8 in
        custom_encoding := Some e;
        e

let camolog = Log.make ["camomile"]

exception Input_encoding of string
exception Output_encoding of string

let recode_tag ?in_enc ?out_enc s =
  try
    let in_enc =
      try match in_enc with Some e -> enc_of_name e | None -> get_encoding ()
      with Unknown_encoding s -> raise (Input_encoding s)
    in
    let out_enc =
      try match out_enc with Some e -> enc_of_name e | None -> C.utf8
      with Unknown_encoding s -> raise (Output_encoding s)
    in
    try C.recode_string ~in_enc ~out_enc s
    with e ->
      let in_enc =
        if in_enc == get_encoding () then
          Printf.sprintf "auto(%s)" (String.concat "," conf_encoding#get)
        else C.name_of in_enc
      in
      camolog#important "Failed to convert %S from %s to %s (%s)!" s in_enc
        (C.name_of out_enc) (Printexc.to_string e);
      s
  with
    | Unknown_encoding e ->
        camolog#important "Failed to convert %S: unknown encoding %s" s e;
        s
    | Input_encoding e ->
        camolog#important "Failed to convert %S: unknown input encoding %s" s e;
        s
    | Output_encoding e ->
        camolog#important "Failed to convert %S: unknown output encoding %s" s e;
        s
    | e ->
        camolog#important "Failed to convert %S: unknown error %s" s
          (Printexc.to_string e);
        s

let env_has key =
  try
    ignore (Sys.getenv key);
    true
  with Not_found -> false

let recode_tag =
  if env_has "LIQ_DISABLE_CAMOMILE" then fun ?in_enc:_ ?out_enc:_ s -> s
  else recode_tag
