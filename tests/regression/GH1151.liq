def f() =
  if process.read.lines('echo "aa\rbb\ncc\r\ndd\r\r\nee"') !=
     ["aa\rbb", "cc", "dd\r", "ee"] then
    test.fail()
  else
    test.pass()
  end
end

test.check(f)
