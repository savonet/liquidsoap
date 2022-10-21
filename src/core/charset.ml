include Charset_base
include Charset_impl

let convert ?source ?target s =
  try convert ?source ?target s
  with e ->
    Charset_base.log#important "Failed to convert %S: %s" s
      (Printexc.to_string e);
    s

let () =
  Lifecycle.before_start (fun () ->
      log#important "Using the %s for charset conversion" description;
      log#info "Supported encodings:";
      log#info "Decoding: %s"
        (String.concat ", " (List.map to_string can_decode));
      log#info "Auto-detect: %s"
        (String.concat ", " (List.map to_string can_decode));
      log#important "Encoding: %s"
        (String.concat ", " (List.map to_string can_encode)))
