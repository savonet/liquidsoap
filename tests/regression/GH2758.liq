def f() =
  s = http.get('http://ifconfig.me/all.json', http_version="1.1")
  test.equals(s.status_message, "OK")
  test.equals(s.status_code, 200)
  test.equals(s.http_version, "1.1")

  let json.parse ({ user_agent } : { user_agent: string}) = s

  test.equals(user_agent, http.user_agent)

  test.pass()
end

test.check(f)
