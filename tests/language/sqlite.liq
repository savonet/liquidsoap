#!../../liquidsoap ../test.liq
def f() =
  # Escaping
  test.equals(sqlite.escape("bla"), "'bla'")
  test.equals(sqlite.escape("a'b"), "'a''b'")

  # Table creation
  fname = "/tmp/test.sql"
  db = sqlite(fname)
  db.exec(
    "DROP TABLE IF EXISTS test"
  )
  db.exec(
    "CREATE TABLE test (n INTEGER, s STRING, f FLOAT);"
  )

  # Insertion
  db.insert(table="test", {n=5, s="bla", f=2.5})
  db.insert(table="test", {n=5, s="bli", f=1.})
  db.insert(table="test", {n=20, s="blu", f=0.2})
  db.insert(table="test", {n=666, s="blu", f=null()})
  db.exec(
    "INSERT INTO test VALUES (5,#{sqlite.escape('hello')},1.2)"
  )

  # Select
  l =
    db.query(
      "SELECT * FROM test WHERE n=5"
    )
  test.equals(list.length(l), 3)
  print(
    "result: #{l}"
  )

  # Iterate
  print(
    "# Iterate"
  )
  db.iter(
    print,
    "SELECT * FROM test WHERE n=5"
  )

  # Table existence
  test.equals(db.table.exists("test"), true)
  test.equals(db.table.exists("ghost"), false)

  # Insertion of lists
  db.exec(
    "CREATE TABLE IF NOT EXISTS abc (a STRING, b STRING, c STRING)"
  )
  db.insert.list(table="abc", [("c", "123"), ("a", "456"), ("b", "789")])

  test.pass()
end

test.check(f)
