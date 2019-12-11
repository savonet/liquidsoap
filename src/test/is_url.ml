let () =
  assert (Http.is_url "http://www.test.com/") ;
  assert (Http.is_url "https://www.test.com/") ;
  assert (not (Http.is_url "/tmp/test"))
