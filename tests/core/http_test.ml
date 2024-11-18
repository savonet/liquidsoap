let () =
  let { Liq_http.user; password } = Liq_http.parse_auth "foo:bar:gni" in
  assert (user = "foo");
  assert (password = "bar:gni")
