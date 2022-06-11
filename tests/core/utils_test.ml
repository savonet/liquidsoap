let () =
  assert (
    Lang_string.escape_utf8_string "aa\240\159\152\133bb"
    = "aa\240\159\152\133bb");
  assert (
    Lang_string.quote_string "aa\240\159\152\133bb" = "\"aa\240\159\152\133bb\"")
