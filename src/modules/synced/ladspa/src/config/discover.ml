module C = Configurator.V1

let () =
  C.main ~name:"ladspa-header" (fun c ->
      let has_ladspa =
        try
          let defined =
            C.C_define.import c ~includes:["ladspa.h"]
              [("LADSPA_VERSION", C.C_define.Type.String)]
          in
          match defined with
            | [("LADSPA_VERSION", C.C_define.Value.String _)] -> true
            | _ -> false
        with _ -> false
      in
      C.C_define.gen_header_file c ~fname:"ocaml_ladspa_config.h"
        [("HAS_LADSPA", C.C_define.Value.Switch has_ladspa)])
