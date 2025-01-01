let () = assert ("aa\\\"bb" = Lang_string.escape_utf8_string "aa\"bb")

(* Bug #4287 *)
let () =
  ignore (Lang_string.Version.of_string "rolling-release-v2.3.x+git@2addd93da")
