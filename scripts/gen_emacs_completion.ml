open Liquidsoap_main.Main

let () =
  run_streams := false;
  load_libs ();
  Lang_string.kprint_string ~pager:false Doc.Value.print_emacs_completions
