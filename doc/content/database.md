# Database support

Liquidsoap supports SQL databases through the sqlite library. If you build
Liquidsoap by yourself, you should install the
[SQLite3-OCaml](https://github.com/mmottl/sqlite3-ocaml) library, e.g. with
`opam install sqlite3`{.bash}.

In order to create or open a database, you should use the `sqlite` function,
which takes as argument the file where the database is stored and returns an
object whose methods can be used to modify or query the database:

```{.liquidsoap include="sqlite.liq" from="open-begin" to="open-end"}

```

table in the database can then be created by calling the `table.create` method
on the object with as arguments the table name (labeled by `table`) and the list
of columns specified by pairs consisting of the column name, and its
type. Setting the `preserve` argument to `true`{.liquidsoap} allows not creating
the table if one already exists under this name. In our example, we want to use
our database to store metadata for files so that we create a table named
`"metadata"`{.liquidsoap} with columns corresponding to the artist, title, etc.:

```{.liquidsoap include="sqlite.liq" from="create-begin" to="create-end"}

```

Inserting a row is then performed using the `insert` method, which takes as
argument the table and a record containing the data for the row:

```{.liquidsoap include="sqlite.liq" from="insert-begin" to="insert-end"}

```

Since the field `filename` is a primary key, it has to be unique (two rows
cannot have the same file name), so that inserting two files with the same
filename in the database will result in an error. If we want that the second
insertion replace the first one, we can pass the `replace=true`{.liquidsoap}
argument to the `insert` function.

We can query the database with the `select` method. For instance, to obtain all
the files whose year is posterior to 2000, we can write

```{.liquidsoap include="sqlite.liq" from="select-begin" to="select-end"}

```

In the case where you want to use strings in your queries, you should always use
`sqlite.escape` to properly escape it and avoid injections:

```{.liquidsoap include="sqlite.liq" from="select2-begin" to="select2-end"}

```

The `select` function, returns a list of rows. To each row will correspond a
list of pairs strings consisting of

- a string: the name of the column,
- a nullable string: its value (this is nullable because the contents of a
  column can be NULL in databases).

We could thus extract the filenames from the above queries and use those in
order to build a playlist as follows:

```{.liquidsoap include="sqlite.liq" from="play-begin" to="play-end"}

```

This can be read as follows: for each row (by `list.map`{.liquidsoap}), we
convert the row to a list of pairs of strings as described above (by calling the
`to_list`{.liquidsoap} method), we replace take the field labeled
`"filename"`{.liquidsoap} (by `list.assoc`{.liquidsoap}) and take its value,
assuming that it is not null (by `null.get`{.liquidsoap}).

Since manipulating rows as lists of pairs of strings is not convenient,
Liquidsoap offers the possibility to represent them as records with
constructions of the form

```liquidsoap
let sqlite.row (r : {a : string; b : int}) = row
```

which instructs to parse the row `row`{.liquidsoap} as a record `r` with fields
`a` and `b` of respective types `string`{.liquidsoap} and
`int`{.liquidsoap}. The above filename extraction is thus more conveniently
written as

```{.liquidsoap include="sqlite.liq" from="play2-begin" to="play2-end"}

```

Other useful methods include

- `count` to count the number of rows satisfying a condition

  ```{.liquidsoap include="sqlite.liq" from="count-begin" to="count-end"}

  ```

- `delete` to delete rows from a table

  ```{.liquidsoap include="sqlite.liq" from="play-begin" to="play-end"}

  ```

- `table.drop` to delete tables from the database

  ```{.liquidsoap include="sqlite.liq" from="drop-begin" to="drop-end"}

  ```

- `exec` to execute an arbitrary SQL query which does not return anything:

  ```{.liquidsoap include="sqlite.liq" from="exec-begin" to="exec-end"}

  ```

- `query` to execute an arbitrary SQL query returning rows

  ```{.liquidsoap include="sqlite.liq" from="query-begin" to="query-end"}

  ```

Finally, if your aim is to index file metadata, you might be interested in the
`medialib.sqlite`{.liquidsoap} operator which is implemented in the standard
library as described above (see the [cookbook](cookbook.html)).
