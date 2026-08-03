(* liquidsoap.build_config.optionals.<dep>: whether each optional dependency
   was available at build time. The list comes from the same table that drives
   the (select ...) stanzas, see gen_dune.ml. *)

let liquidsoap_build_config_optionals =
  Lang.add_module ~base:Liquidsoap_lang.Builtins_lang.liquidsoap_build_config
    "optionals"

let () =
  List.iter
    (fun (name, value) ->
      ignore
        (Lang.add_builtin_base ~category:`Configuration
           ~descr:("Build-time configuration for " ^ name)
           ~base:liquidsoap_build_config_optionals name (`Bool value)
           Lang.bool_t))
    Optionals_build_config.all
