%ifdef sqlite
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
        separator=", ",
        columns
      )
    values =
      string.concat(
        separator=", ",
        values
      )
    sql =
      "INSERT INTO #{sqlite.escape(table)} (#{columns}) VALUES (#{values})"

    db.exec(sql)
  end

  def db.select(~table, what="*", ~where="", ~limit=null) =
    where =
      if
        where == ""
      then
        ""
      else
        " WHERE #{where}"
      end
    limit =
      if
        null.defined(limit)
      then
        " LIMIT #{null.get(limit)}"
      else
        ""
      end
    db.query(
      "SELECT #{what} FROM #{table}#{where}#{limit}"
    )
  end

  def db.select.iter(f, ~table, what="*", ~where="") =
    where =
      if
        where == ""
      then
        ""
      else
        " WHERE #{where}"
      end
    db.iter(
      f,
      "SELECT #{what} FROM #{table}#{where}"
    )
  end

  def db.delete(~table, ~where="") =
    where =
      if
        where == ""
      then
        ""
      else
        " WHERE #{where}"
      end
    db.exec(
      "DELETE FROM #{table}#{where}"
    )
  end

  # Count the number of items matching a condition.
  def db.count(~table, ~where="") =
    where =
      if
        where == ""
      then
        ""
      else
        " WHERE " ^
          where
      end
    let sqlite.query ([{count}] : [{count: int}]) =
      db.query(
        "SELECT COUNT(*) AS count FROM #{table}#{where}"
      )
    count
  end

  # Operations on tables.
  let db.table = ()

  def db.table.drop(table, ~existing=true) =
    existing =
      if
        existing
      then
        " IF EXISTS"
      else
        ""
      end
    db.exec(
      "DROP TABLE#{existing} #{table}"
    )
  end

  # Create a table with given fields.
  # @param ~preserve Preserve previous table if one already exists under the same name.
  def db.table.create(table, ~preserve=false, fields) =
    preserve =
      if
        preserve
      then
        " IF NOT EXISTS"
      else
        ""
      end
    fields =
      string.concat(
        separator=", ",
        list.map(
          fun (lt) ->
            "#{fst(lt)} #{snd(lt)}",
          fields
        )
      )
    db.exec(
      "CREATE TABLE#{preserve} #{table} (#{fields})"
    )
  end

  # Check whether a table exists.
  def db.table.exists(name) =
    let sqlite.query ([{name}] : [{name: string}]) =
      db.query(
        "SELECT name FROM sqlite_master WHERE type='table' AND name=#{
          sqlite.escape(name)
        }"
      )
    name
  end

  db
end
%endif
