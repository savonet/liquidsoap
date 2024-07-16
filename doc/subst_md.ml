let variables = [("LIQUIDSOAP_VERSION", Liquidsoap_lang.Build_config.version)]

let () =
  let f = Sys.argv.(1) in
  let fd = open_in f in
  let len = in_channel_length fd in
  let b = Bytes.create len in
  really_input fd b 0 len;
  print_string
    (List.fold_left
       (fun cur (lbl, v) ->
         Pcre.substitute
           ~rex:(Pcre.regexp ("@" ^ lbl ^ "@"))
           ~subst:(fun _ -> v)
           cur)
       (Bytes.unsafe_to_string b) variables)
