#!../../liquidsoap ../test.liq
def f() =
  fname = "/tmp/test.sql"
  file.remove(fname)
  db = sqlite(fname)
  db.exec(
    "CREATE TABLE test (n INTEGER PRIMARY KEY, s STRING, f FLOAT);"
  )
  db.insert(table="test", {n=5, s="bla", f=2.5})
  test.pass()
end

test.check(f)
