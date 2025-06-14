#!../../liquidsoap ../test.liq
def f() =
%ifndef sqlite
  print(
    "WARNING: Not compiled with sqlite support. Passing test."
  )
%else
  # Escaping
  test.equal(sqlite.escape("bla"), "'bla'")
  test.equal(sqlite.escape("a'b"), "'a''b'")

  # Table creation
  tmp = file.temp("test", ".sql")
  on_cleanup({file.remove(tmp)})
  db = sqlite(tmp)
  db.table.drop("test")
  db.exec(
    "CREATE TABLE test (n INTEGER, s STRING, f FLOAT);"
  )
  db.table.create(
    "test2",
    preserve=true,
    [
      (
        "id",
        "INTEGER UNIQUE"
      ),
      ("value", "STRING")
    ]
  )

  # Insertion
  db.insert(table="test", {n=5, s="bla", f=2.5})
  db.insert(table="test", {n=5, s="bli", f=1.})
  db.insert(table="test", {n=20, s="blu", f=0.2})
  db.insert(table="test", {n=666, s="blu", f=null})
  db.exec(
    "INSERT INTO test VALUES (5,#{sqlite.escape('hello')},1.2)"
  )

  db.insert(table="test2", replace=true, {id=0, value="a"})
  db.insert(table="test2", replace=true, {id=0, value="b"})

  # Count
  test.equal(db.count(table="test", where="n=5"), 3)

  # Select
  l = db.select(table="test", where="n=5")
  test.equal(list.length(l), 3)
  print(
    "result: #{l}"
  )

  # Iterate
  print(
    "# Iterate"
  )
  db.select.iter(print, table="test", where="n=5")

  # Delete
  db.delete(table="test2", where="id=0")
  db.delete(table="test2", where="id=55")

  # # Table existence
  # test.equal(db.table.exists("test"), true)
  # test.equal(db.table.exists("ghost"), false)

  # Insertion of lists
  db.table.create(
    "abc", preserve=true, [("a", "STRING"), ("b", "STRING"), ("c", "STRING")]
  )
  db.insert.list(table="abc", [("c", "123"), ("a", "456"), ("b", "789")])

  # Query parse
  let sqlite.query ([{a}] : [{a: int}]) =
    db.query(
      "SELECT a FROM abc LIMIT 1"
    )
  test.equal(a, 456)

  # Manual row manipulation
  let [row] =
    db.query(
      "SELECT a FROM abc LIMIT 1"
    )
  let [(label, v)] = row.to_list()
  test.equal(label, "a")
  test.equal(v, "456")
  let sqlite.row (r : {a: string, b: string}) = row
  test.equal(r.a, "456")
%endif

  test.pass()
end

test.check(f)
