#!../../liquidsoap ../test.liq
def f() =
  fname = "/tmp/test.sql"
  db = sqlite(fname)
  db.exec(
    "DROP TABLE IF EXISTS test"
  )
  db.exec(
    "CREATE TABLE test (n INTEGER, s STRING, f FLOAT);"
  )
  db.insert(table="test", {n=5, s="bla", f=2.5})
  db.insert(table="test", {n=5, s="bli", f=1.})
  db.insert(table="test", {n=6, s="blu", f=0.2})
  l =
    db.query(
      "SELECT * FROM test WHERE n=5"
    )
  print(
    "result: #{l}"
  )
  test.pass()
end

test.check(f)
