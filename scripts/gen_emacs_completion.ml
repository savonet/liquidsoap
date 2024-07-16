open Liquidsoap_runtime

let () =
  Main.parse_options ();
  Main.with_toplevel ~run_streams:false (fun () ->
      Lang_string.kprint_string ~pager:false Doc.Value.print_emacs_completions)
