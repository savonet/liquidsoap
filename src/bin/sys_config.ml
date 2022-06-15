module C = Configurator.V1

let () =
  C.main ~name:"liquidsoap" (fun c ->
      let oc = open_out "sys_config.dune.ml" in
      output_string oc
        (Printf.sprintf {|
let ext_exe = %S
let host = %S
|}
           (Option.value ~default:"" (C.ocaml_config_var c "ext_ext"))
           (C.ocaml_config_var_exn c "system"));
      close_out oc)
