let srt_available =
  Array.length Sys.argv > 2 && String.trim Sys.argv.(2) = "true"

let () =
  let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "detect" in
  match mode with
    | "detect" ->
        if srt_available then
          print_string
            {|(executables
 (names detect check)
 (libraries dune-configurator unix))

(rule
 (targets srt_available srt_c_flags.sexp srt_c_flags srt_c_library_flags.sexp)
 (action
  (run ./detect.exe --os-type %{os_type} --context %{context_name} srt)))
|}
        else
          print_string
            {|(rule
 (targets srt_available)
 (action
  (write-file %{targets} "false\n")))

(rule
 (targets srt_c_flags.sexp)
 (action
  (write-file %{targets} "()")))

(rule
 (targets srt_c_flags)
 (action
  (write-file %{targets} "")))

(rule
 (targets srt_c_library_flags.sexp)
 (action
  (write-file %{targets} "()")))
|}
    | "generator" when not srt_available -> ()
    | "generator" ->
        print_string
          {|(rule
 (targets gen_stubs.exe)
 (deps
  (:build_native ./build_native.sh)
  (:ml ./gen_stubs.ml)
  (glob_files ../../src/constants/.srt_constants.objs/byte/*.cmi)
  (glob_files ../../src/constants/srt_constants.a)
  (glob_files ../../src/types/.srt_types.objs/byte/*.cmi)
  (glob_files ../../src/types/srt_types.a)
  (glob_files ../../src/stubs/.srt_stubs.objs/byte/*.cmi)
  (glob_files ../../src/stubs/srt_stubs.a)
  (glob_files ../../src/stubs/locked/.srt_stubs_locked.objs/byte/*.cmi)
  (glob_files
   ../../src/stubs/locked/.srt_stubs_locked.objs/native/srt_stubs_locked.o))
 (action
  (run
   %{build_native}
   %{ocaml-config:system}
   %{ml}
   %{targets}
   -thread
   -package
   ctypes.foreign
   -I
   ../../src/constants/.srt_constants.objs/byte/
   -I
   ../../src/types/.srt_types.objs/byte/
   -I
   ../../src/stubs/.srt_stubs.objs/byte/
   -I
   ../../src/stubs/locked/.srt_stubs_locked.objs/byte/
   %{lib-private:srt.constants:srt_constants.cmxa}
   %{lib-private:srt.types:srt_types.cmxa}
   %{lib-private:srt.stubs:srt_stubs.cmxa}
   %{lib-private:srt.stubs:locked/.srt_stubs_locked.objs/native/srt_stubs_locked.cmx})))

(rule
 (targets gen_types_c.exe)
 (deps
  (:build_native ./build_native.sh)
  (:ml ./gen_types_c.ml)
  (glob_files ../../src/constants/.srt_constants.objs/byte/*.cmi)
  (glob_files ../../src/constants/srt_constants.a)
  (glob_files ../../src/types/.srt_types.objs/byte/*.cmi)
  (glob_files ../../src/types/srt_types.a))
 (action
  (run
   %{build_native}
   %{ocaml-config:system}
   %{ml}
   %{targets}
   -I
   ../../src/constants/.srt_constants.objs/byte/
   -I
   ../../src/types/.srt_types.objs/byte/
   %{lib-private:srt.constants:srt_constants.cmxa}
   %{lib-private:srt.types:srt_types.cmxa})))

(rule
 (targets gen_types.c)
 (deps
  (:exec ./exec.sh)
  (:gen ./gen_types_c.exe))
 (action
  (system "./%{exec} %{ocaml-config:system} ./%{gen} %{targets}")))

(rule
 (targets ctypes_dir)
 (action
  (with-stdout-to
   %{targets}
   (run dirname %{lib:ctypes:ctypes_cstubs_internals.h}))))

(rule
 (targets gen_types_c_target.exe)
 (deps
  (:c_flags ../../detect/srt_c_flags)
  (:ctypes_dir ./ctypes_dir)
  (:c_code ./gen_types.c))
 (action
  (run
   %{ocaml-config:c_compiler}
   -I
   %{read-lines:ctypes_dir}
   -I
   %{ocaml-config:standard_library}
   %{read-lines:../../detect/srt_c_flags}
   -o
   %{targets}
   %{c_code})))

(rule
 (targets gen_constants_c.exe)
 (deps
  (:build_native ./build_native.sh)
  (:ml gen_constants_c.ml)
  (glob_files ../../src/constants/.srt_constants.objs/byte/*.cmi)
  (glob_files ../../src/constants/srt_constants.a))
 (action
  (run
   %{build_native}
   %{ocaml-config:system}
   %{ml}
   %{targets}
   -I
   ../../src/constants/.srt_constants.objs/byte/
   %{lib-private:srt.constants:srt_constants.cmxa})))

(rule
 (targets gen_constants.c)
 (deps
  (:exec ./exec.sh)
  (:gen ./gen_constants_c.exe))
 (action
  (system "./%{exec} %{ocaml-config:system} ./%{gen} %{targets}")))

(rule
 (targets gen_constants_c_target.exe)
 (deps
  (:c_flags ../../detect/srt_c_flags)
  (:ctypes_dir ./ctypes_dir)
  (:c_code ./gen_constants.c))
 (action
  (run
   %{ocaml-config:c_compiler}
   -I
   %{read-lines:ctypes_dir}
   -I
   %{ocaml-config:standard_library}
   %{read-lines:../../detect/srt_c_flags}
   -o
   %{targets}
   %{c_code})))
|}
    | "types" when not srt_available -> ()
    | "types" ->
        print_string
          {|(rule
 (targets srt_generated_constants.ml)
 (deps
  (:exec ../../generator/rules/exec.sh)
  (:gen ../../generator/rules/gen_constants_c_target.exe))
 (action
  (with-stdout-to
   %{targets}
   (system "%{exec} %{ocaml-config:system} %{gen}"))))
|}
    | "stubs" when not srt_available -> ()
    | "stubs" ->
        print_string
          {|(rule
 (targets srt_generated_types.ml)
 (deps
  (:exec ../../generator/rules/exec.sh)
  (:gen ../../generator/rules/gen_types_c_target.exe))
 (action
  (with-stdout-to
   %{targets}
   (system "%{exec} %{ocaml-config:system} %{gen}"))))
|}
    | "src" when not srt_available ->
        print_string
          {|(rule
 (targets srt_generated_stubs.c)
 (action
  (write-file %{targets} "")))

(rule
 (targets srt_generated_stubs_locked.c)
 (action
  (write-file %{targets} "")))
|}
    | "src" ->
        print_string
          {|(rule
 (targets srt_generated_stubs.ml)
 (deps
  (:exec ../generator/rules/exec.sh)
  (:gen ../generator/rules/gen_stubs.exe))
 (action
  (system "%{exec} %{ocaml-config:system} %{gen} ml %{targets}")))

(rule
 (targets srt_generated_stubs.c)
 (deps
  (:exec ../generator/rules/exec.sh)
  (:gen ../generator/rules/gen_stubs.exe))
 (action
  (system "%{exec} %{ocaml-config:system} %{gen} c %{targets}")))

(rule
 (targets srt_generated_stubs_locked.ml)
 (deps
  (:exec ../generator/rules/exec.sh)
  (:gen ../generator/rules/gen_stubs.exe))
 (action
  (system "%{exec} %{ocaml-config:system} %{gen} ml %{targets} locked")))

(rule
 (targets srt_generated_stubs_locked.c)
 (deps
  (:exec ../generator/rules/exec.sh)
  (:gen ../generator/rules/gen_stubs.exe))
 (action
  (system "%{exec} %{ocaml-config:system} %{gen} c %{targets} locked")))
|}
    | _ -> ()
