let version = "0.3.6"
let conf = Dtools.Conf.void "Liquidsoap configuration"
let var_script = ref "default"
let substs = ref
    [
      "<script>", (fun () -> !var_script);
      "<pid>", (fun () -> string_of_int (Unix.getpid ()));
      "<home>", (fun () -> Sys.getenv "HOME");
    ]
let add_subst r s = substs := (r, fun () -> s) :: !substs
let subst_vars s =
  List.fold_left
    (fun v (r, s) -> Str.global_replace (Str.regexp r) (s ()) v)
     s !substs
let env_has key = try ignore (Sys.getenv key) ; true with Not_found -> false
let dynliq_option = []
let resample = None
module C = CamomileLibrary.Default.Camomile.CharEncoding
let enc_of_name s =
  try C.of_name s with
    | Not_found -> failwith (Printf.sprintf "Unknown encoding %s !" s)

let conf_tag =
  Dtools.Conf.void ~p:(conf#plug "tag") "Settings related to metadata tags"
let conf_encoding =
  Dtools.Conf.list ~p:(conf_tag#plug "encodings") ~d:["UTF-8";"ISO-8859-1"]
    "List of encodings to try for automatic encoding detection"

let custom_encoding = ref None

let get_encoding () =
  match !custom_encoding with
    | Some e -> e
    | None ->
        let encs = conf_encoding#get in
	let e = C.automatic "LIQ-TAGS" (List.map enc_of_name encs) C.utf8 in
        custom_encoding := Some e ;
	e

let recode_tag ?encoding s =
  let in_enc =
    match encoding with
      | Some e -> enc_of_name e
      | None -> get_encoding ()
  in
    C.recode_string ~in_enc ~out_enc:C.utf8 s

let recode_tag =
  if env_has "LIQ_DISABLE_CAMOMILE" then
    fun ?encoding s -> s
  else
    recode_tag
let requests_max_id = 50
let requests_table_size = 50
let tts_program = "/usr/local/lib/liquidsoap/liquidtts"
let rundir = "/usr/local/var/run/liquidsoap"
let logdir = "/usr/local/var/log/liquidsoap"
let libs_dir = "/usr/local/lib/liquidsoap/0.3.6"
let display_types = ref false
let () = add_subst "<sysrundir>" "/usr/local/var/run/liquidsoap"
let () = add_subst "<syslogdir>" "/usr/local/var/log/liquidsoap"
