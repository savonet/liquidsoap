#!../../liquidsoap ../test.liq
def f() =
  fname = "/tmp/test.sql"
  file.remove(fname)
  db = sqlite(fname)
  db.exec(
    "CREATE TABLE test (n INTEGER PRIMARY KEY, s STRING, f FLOAT);"
  )
  test.pass()
end

test.check(f)
