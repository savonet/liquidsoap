def f() =
  config = null
  port = 123

  parsed = if null.defined(config) then {port=null} else {port=null} end

  port = parsed.port ?? port

  print(
    "Port: #{port + 1}"
  )

  test.pass()
end

test.check(f)
