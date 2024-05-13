open Liquidsoap_runtime

let () =
  Main.with_eval (fun () ->
      Lang_string.kprint_string ~pager:false Doc.Value.print_emacs_completions)
