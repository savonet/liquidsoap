# @docof sqlite
def replaces sqlite(file) =
  db = sqlite(file)

  # Insert a list of key / value pairs, where all the values are
  # strings.
  def db.insert.list(~table, entries) =
    let (columns, values) =
      list.fold(
        fun (result, entry) ->
          begin
            let (columns, values) = result
            let (column, value) = entry
            ([...columns, column], [...values, sqlite.escape(value)])
          end,
        ([], []),
        entries
      )

    columns =
      string.concat(
        separator=
          ", ",
        columns
      )
    values =
      string.concat(
        separator=
          ", ",
        values
      )
    sql =
      "INSERT INTO #{sqlite.escape(table)} (#{columns}) VALUES (#{values})"

    db.exec(sql)
  end

  # Operations on tables.
  let db.table = ()

  def db.table.exists(name) =
    let [[(_, n)]] =
      db.query(
        "SELECT name FROM sqlite_master WHERE type='table' AND name=#{
          sqlite.escape(name)
        }"
      )
    null.defined(n)
  end

  db
end
