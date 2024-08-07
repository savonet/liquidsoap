let () =
  assert (Liq_http.is_url "http://www.test.com/");
  assert (Liq_http.is_url "https://www.test.com/");
  assert (not (Liq_http.is_url "/tmp/test"))
