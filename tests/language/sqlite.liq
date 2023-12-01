#!../../liquidsoap ../test.liq
def f() =
  test.equals(sqlite.escape("bla"), "'bla'")
  test.equals(sqlite.escape("a'b"), "'a''b'")

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
  db.insert(table="test", {n=66, s="blu", f=0.2})
  db.exec(
    "INSERT INTO test VALUES (5,#{sqlite.escape('hello')},1.2)"
  )
  l =
    db.query(
      "SELECT * FROM test WHERE n=5"
    )
  test.equals(list.length(l), 3)
  print(
    "result: #{l}"
  )
  test.equals(db.table.exists("test"), true)
  test.equals(db.table.exists("ghost"), false)
  test.pass()
end

test.check(f)
