module C = Configurator.V1

let () =
  C.main ~name:"datachannel-config" (fun _c ->
      let cflags =
        [
          "-I";
          "/Users/toots/sources/libdatachannel/include";
          "-I";
          "/home/smimram/build/libdatachannel/include";
        ]
      in
      let libs =
        [
          "-L";
          "/Users/toots/sources/libdatachannel/build";
          "-L";
          "/home/smimram/build/libdatachannel/build";
          "-ldatachannel";
        ]
      in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" libs;
      (* Raw flags for use in (system ...) shell commands *)
      let write_lines file flags =
        let oc = open_out file in
        List.iter
          (fun f ->
            output_string oc f;
            output_char oc '\n')
          flags;
        close_out oc
      in
      write_lines "c_flags" cflags)
