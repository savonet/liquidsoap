(* The typing environment dumps that liquidsoap writes and the language server
   reads are marshaled, so a reader and a writer must agree on their shape. The
   digests below cover the files that define it: when one changes, either the
   shape is the same and the digest is promoted, or
   [Jsoo_safe_env.abi_version] is bumped so that older dumps are rejected. *)
let () =
  Printf.printf "abi_version %d\n"
    Liquidsoap_lang_types.Jsoo_safe_env.abi_version;
  Array.iteri
    (fun i file ->
      if i > 0 then
        Printf.printf "%s %s\n" (Filename.basename file)
          (Digest.to_hex (Digest.file file)))
    Sys.argv
