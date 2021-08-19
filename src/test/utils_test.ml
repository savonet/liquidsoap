let () =
  assert (Utils.escape_utf8 "aa\240\159\152\133bb" = "\"aa\240\159\152\133bb\"")
