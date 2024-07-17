let version = "0.3.5"
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
let recode_tag ?encoding s = s
let requests_max_id = 50
let requests_table_size = 50
let tts_program = "/usr/local/lib/liquidsoap/liquidtts"
let rundir = "/usr/local/var/run/liquidsoap"
let logdir = "/usr/local/var/log/liquidsoap"
let libs_dir = "/usr/local/lib/liquidsoap/0.3.5"
let display_types = ref false
let () = add_subst "<sysrundir>" "/usr/local/var/run/liquidsoap"
let () = add_subst "<syslogdir>" "/usr/local/var/log/liquidsoap"
