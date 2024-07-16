def f() =
  s = http.get('http://ifconfig.me/all.json', http_version="1.1")
  test.equal(s.status_message, "OK")
  test.equal(s.status_code, 200)
  test.equal(s.http_version, "1.1")
  let json.parse ({user_agent} : {user_agent: string}) = s
  test.equal(user_agent, http.user_agent)
  test.pass()
end

test.check(f)
