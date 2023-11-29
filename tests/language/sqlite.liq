#!../../liquidsoap ../test.liq
def f() =
  db = sqlite("test.sql")
  db.exec(
    "CREATE TABLE test (n INTEGER, s STRING, f FLOAT);"
  )
  test.pass()
end

test.check(f)
