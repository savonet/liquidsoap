module C = Configurator.V1

external is_big_endian : unit -> bool = "ocaml_mm_is_big_endian"

let () =
  C.main ~name:"flac-config" (fun c ->
      C.C_define.gen_header_file c ~fname:"flac_config.h"
        [("BIGENDIAN", Switch (is_big_endian ()))])
