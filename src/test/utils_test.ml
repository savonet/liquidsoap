let () =
  assert (
    String_utils.escape_utf8_string "aa\240\159\152\133bb"
    = "aa\240\159\152\133bb");
  assert (
    String_utils.quote_string "aa\240\159\152\133bb"
    = "\"aa\240\159\152\133bb\"")
