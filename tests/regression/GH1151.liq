def f() =
  test.equal(
    process.read.lines(
      'printf "aa\rbb\ncc\r\ndd\r\r\nee"'
    ),
    ["aa\rbb", "cc", "dd\r", "ee"]
  )
  test.pass()
end

test.check(f)
