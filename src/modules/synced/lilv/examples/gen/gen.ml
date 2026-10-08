let () =
  if Sys.argv.(1) = "true" then
    print_string
      {|(executables
 (names amp inspect)
 (modules amp inspect)
 (libraries lilv))

(rule
 (alias runtest)
 (action
  (run ./inspect.exe)))
|}
