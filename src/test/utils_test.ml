let () =
  assert (
    Utils.escape_utf8_string "aa\240\159\152\133bb" = "aa\240\159\152\133bb");
  assert (
    Utils.quote_utf8_string "aa\240\159\152\133bb" = "\"aa\240\159\152\133bb\"")
