# A library to store the metadata of files in given folders and query them. This
# is useful to generate playlists based on metadata.
# @category File
# @param ~persistency Store the database in given file, which is reuse to populate the database on next run.
# @param ~refresh Scan directories for new files every given number of seconds (by default the database is never updated).
# @param ~standardize Function mapped on metadata when indexing. It can be used to change the field names to standard ones, pretreat data, etc.
# @param ~initial_progress Show progress of library being indexed at startup.
# @param ~directories Directories to look for files in.
# @param dir Directory to look for files in.
# @method find Find files according to conditions on metadata.
# @method refresh Update metadatas and look for new files.
# @method add_directory Add a new directory which should be scanned.
# @method clear Remove all known metadata.
def medialib(
  ~id=null,
  ~persistency=null,
  ~refresh=null,
  ~standardize=fun (m) -> m,
  ~initial_progress=true,
  ~directories=[],
  dir=null
) =
  id = string.id.default(default="medialib", id)
  refresh_time = refresh
  directories = ref(directories)
  if null.defined(dir) then directories := null.get(dir)::directories() end
  db = ref([])

  def dt(t) =
    string.float(decimal_places=2, time() - t)
  end

  # Read metadata from file.
  def metadata(f) =
    m = file.metadata.native(f)
    m = standardize(m)

    # Sanitize
    m = metadata.cover.remove(m)
    m = list.assoc.filter(fun (k, _) -> not list.mem(k, ["priv", "rva2"]), m)

    # Add more metadata
    m = ("basename", path.basename(f))::m
    m =
      (
        "last scan",
        string.float(time())
      )::m
    m
  end

  # Whether an entry needs to be updated.
  def needs_update(f, m) =
    file.mtime(f) >
      string.to_float(
        m[
          "last scan"
        ]
      )
  end

  # Add a file to the database.
  def add(f) =
    # If file doesn't exist remove it
    if
      not (file.exists(f))
    then
      db := list.assoc.remove(f, db())
    else
      # New file or not recent enough metadata
      if
        not list.assoc.mem(f, db()) or needs_update(f, list.assoc(f, db()))
      then
        db := (f, metadata(f))::list.assoc.remove(f, db())
      end
    end
  end

  # Update database by renewing metadata and removing removed files.
  def update(~progress=fun (_, _) -> ()) =
    len = list.length(db())
    n = ref(0)
    nu = ref(0)

    def u(fm) =
      let (f, m) = fm
      ref.incr(n)
      progress(n(), len)
      if
        not (file.exists(f))
      then
        null
      elsif
        needs_update(f, m)
      then
        ref.incr(nu)
        (f, metadata(f))
      else
        (f, m)
      end
    end

    db := list.filter_map(u, db())
    log.debug(
      label=id,
      "Updated #{nu()} files."
    )
  end

  # Make sure that new files from directories are registered.
  def scan(~progress=fun (_, _) -> ()) =
    l =
      list.map(
        fun (d) -> file.ls(absolute=true, recursive=true, d),
        directories()
      )

    l = list.flatten(l)
    n = ref(0)
    len = list.length(l)

    def add(f) =
      ref.incr(n)
      progress(n(), len)
      add(f)
    end

    list.iter(add, l)
  end

  # Increment when the format of the db changes
  db_version = 1

  # Load from the persistent file.
  def load() =
    db := []
    if
      null.defined(persistency)
    then
      f = null.get(persistency)
      if
        file.exists(f)
      then
        try
          let json.parse ((v, parsed) :
            (int * [(string * [(string * string)])]?)
          ) = file.contents(f)

          if
            v == db_version and null.defined(parsed)
          then
            db := null.get(parsed)
          end
        catch e do
          log.important(
            label=id,
            "Failed to parse persistent file #{f}: #{e.kind}: #{e.message}"
          )
        end
      end
    end
  end

  # Store the file in a persistent file.
  def store() =
    if
      null.defined(persistency)
    then
      f = null.get(persistency)
      data = json.stringify(compact=true, (db_version, db()))
      file.write(data=data, f)
      log.info(
        label=id,
        "Wrote persistent file #{f}"
      )
    end
  end

  # Refresh the library.
  def refresh() =
    log.info(
      label=id,
      "Refreshing the library..."
    )
    t = time()
    update()
    scan()
    store()
    log.info(
      label=id,
      "Refreshed the library in #{dt(t)}s."
    )
  end

  # Find all files matching given criteria.
  def find(
    ~case_sensitive=true,
    ~artist=null,
    ~artist_contains=null,
    ~artist_matches=null,
    ~album=null,
    ~genre=null,
    ~title=null,
    ~title_contains=null,
    ~filename=null,
    ~filename_contains=null,
    ~filename_matches=null,
    ~year=null,
    ~year_ge=null,
    ~year_lt=null,
    ~bpm=null,
    ~bpm_ge=null,
    ~bpm_lt=null,
    ~predicate=(fun (_) -> true)
  ) =
    def p(m) =
      def eq(s, t) =
        if case_sensitive then s == t else string.case(s) == string.case(t) end
      end

      def contains(s, t) =
        if
          case_sensitive
        then
          string.contains(substring=s, t)
        else
          string.contains(substring=string.case(s), string.case(t))
        end
      end

      def eqf(k, v) =
        null.defined(v) ? eq(m[k], null.get(v)) : true
      end

      def ctf(k, v) =
        null.defined(v) ? contains(null.get(v), m[k]) : true
      end

      def mtf(k, v) =
        null.defined(v) ? string.match(pattern=null.get(v), m[k]) : true
      end

      eqf("artist", artist)
    and
      ctf("artist", artist_contains)
    and
      mtf("artist", artist_matches)
    and
      eqf("album", album)
    and
      eqf("genre", genre)
    and
      eqf("title", title)
    and
      ctf("title", title_contains)
    and
      eqf("filename", filename)
    and
      ctf("basename", filename_contains)
    and
      mtf("basename", filename_matches)
    and

        if
          null.defined(year) or null.defined(year_ge) or null.defined(year_lt)
        then
          if
            string.is_int(m["year"])
          then
            y = string.to_int(m["year"])
            (null.defined(year) ? y == null.get(year) : true )
          and
            (null.defined(year_ge) ? y >= null.get(year_ge) : true )
          and
            (null.defined(year_lt) ? y < null.get(year_lt) : true )
          else
            false
          end
        else
          true
        end

    and

        if
          null.defined(bpm) or null.defined(bpm_ge) or null.defined(bpm_lt)
        then
          if
            string.is_int(m["bpm"])
          then
            b = string.to_int(m["bpm"])
            (null.defined(bpm) ? b == null.get(bpm) : true )
          and
            (null.defined(bpm_ge) ? b >= null.get(bpm_ge) : true )
          and
            (null.defined(bpm_lt) ? b < null.get(bpm_lt) : true )
          else
            false
          end
        else
          true
        end

    and
      predicate(m)
    end

    l = list.filter(fun (fm) -> p(snd(fm)), db())
    l = list.map(fst, l)
    l
  end

  t = time()
  load()
  log.important(
    label=id,
    "Loaded library from #{persistency} in #{dt(t)}s: #{list.length(db())} \
     entries"
  )

  t = time()
  progress =
    if
      initial_progress
    then
      fun (n, l) ->
        print(
          newline=false,
          "#{id}: updating #{n * 100 / l}%...\r"
        )
    else
      fun (_, _) -> ()
    end

  update(progress=progress)
  log.important(
    label=id,
    "Updated library in #{dt(t)}s: #{list.length(db())} entries"
  )

  t = time()
  progress =
    if
      initial_progress
    then
      fun (n, l) ->
        print(
          newline=false,
          "#{id}: scanning #{n * 100 / l}%...\r"
        )
    else
      fun (_, _) -> ()
    end

  scan(progress=progress)
  log.important(
    label=id,
    "Scanned new files in #{dt(t)}s: #{list.length(db())} entries"
  )

  store()
  log.important(
    label=id,
    "Stored library"
  )
  if
    null.defined(refresh_time)
  then
    thread.run(
      delay=null.get(refresh_time),
      every=null.get(refresh_time),
      refresh
    )
  end

  def clear() =
    db := []
  end

  def add_directory(d) =
    directories := d::directories()
    scan()
  end

  {find=find, refresh=refresh, add_directory=add_directory, clear=clear}
end

%ifdef sqlite
# A library to store the metadata of files in given folders and query
# them. This is useful to generate playlists based on metadata. This version
# use an SQL implementation which should be much faster and less memory
# consuming than the basic one.
# @category File
# @param ~persistency Store the database in given file, which is reuse to populate the database on next run.
# @param ~refresh Scan directories for new files every given number of seconds (by default the database is never updated).
# @param ~standardize Function mapped on metadata when indexing. It can be used to change the field names to standard ones, pretreat data, etc.
# @param ~initial_progress Show progress of library being indexed at startup.
# @param ~directories Directories to look for files in.
# @param dir Directory to look for files in.
# @method find Find files according to conditions on metadata.
# @method refresh Update metadatas and look for new files.
# @method add_directory Add a new directory which should be scanned.
# @method clear Remove all known metadata.
def medialib.sqlite(
  ~id=null,
  ~database,
  ~refresh=null,
  ~standardize=fun (m) -> m,
  ~initial_progress=true,
  ~directories=[],
  dir=null
) =
  id = string.id.default(default="medialib.sqlite", id)
  refresh_time = refresh
  directories = ref(directories)
  if null.defined(dir) then directories := null.get(dir)::directories() end

  fields_string = ["artist", "title", "album", "genre", "basename"]
  fields_int = ["year", "bpm"]
  fields_float = ["last_scan"]

  db = sqlite(database)
  begin
    fields_string = list.map(fun (l) -> (l, "STRING"), fields_string)
    fields_int = list.map(fun (l) -> (l, "INT"), fields_int)
    fields_float = list.map(fun (l) -> (l, "FLOAT"), fields_float)
    db.table.create(
      "metadata",
      preserve=true,
      [
        (
          "file",
          "STRING PRIMARY KEY"
        ),
        ...fields_string,
        ...fields_int,
        ...fields_float
      ]
    )
  end

  def dt(t) =
    string.float(decimal_places=2, time() - t)
  end

  # Read metadata from file.
  def metadata(f) =
    m = file.metadata.native(f)
    m = standardize(m)

    # Sanitize
    m = metadata.cover.remove(m)
    m = list.assoc.filter(fun (k, _) -> not list.mem(k, ["priv", "rva2"]), m)

    # Add more metadata
    m = ("basename", path.basename(f))::m
    m = ("last_scan", string.float(time()))::m
    m
  end

  # Whether an entry needs to be updated.
  def needs_update(f, last_scan) =
    file.mtime(f) > last_scan
  end

  # Remove file from the database
  def remove(f) =
    db.delete(table="metadata", where="file=#{sqlite.escape(f)}")
  end

  # Add a file to the database.
  def add(f) =
    # If file doesn't exist remove it
    if
      not (file.exists(f))
    then
      remove(f)
    else
      count = db.count(table="metadata", where="file=#{sqlite.escape(f)}")
      def last_scan() =
        let sqlite.query ([{last_scan}] : [{last_scan: float}]) =
          db.select(
            "last_scan",
            table="metadata",
            where="file=#{sqlite.escape(f)}"
          )
        last_scan
      end

      # New file or not recent enough metadata
      if
        count == 0 or needs_update(f, last_scan())
      then
        m = metadata(f)
        def field(~map, k) =
          def map(x) =
            # Harden
            try
              map(x)
            catch _ do
              null
            end
          end
          if list.assoc.mem(k, m) then map(list.assoc(k, m)) else null end
        end
        id = fun (x) -> x
        m =
          {
            file=f,
            artist=field(map=id, "artist"),
            title=field(map=id, "title"),
            album=field(map=id, "album"),
            genre=field(map=id, "genre"),
            basename=field(map=id, "basename"),
            last_scan=field(map=float_of_string, "last_scan"),
            year=field(map=int_of_string, "year"),
            bpm=field(map=int_of_string, "bpm")
          }
        db.insert(table="metadata", replace=true, m)
      end
    end
  end

  # Number of entries in the database
  def count() =
    db.count(table="metadata")
  end

  # Update database by renewing metadata and removing removed files.
  def update(~progress=fun (_, _) -> ()) =
    len = count()
    n = ref(0)
    nu = ref(0)

    def u(row) =
      ref.incr(n)
      progress(n(), len)
      let sqlite.row ({file} : {file: string}) = row
      add(file)
    end

    db.select.iter(u, "file", table="metadata")
    log.debug(
      label=id,
      "Updated #{nu()} files."
    )
  end

  # Make sure that new files from directories are registered.
  def scan(~progress=fun (_, _) -> ()) =
    l =
      list.map(
        fun (d) -> file.ls(absolute=true, recursive=true, d),
        directories()
      )

    l = list.flatten(l)
    n = ref(0)
    len = list.length(l)

    def add(f) =
      ref.incr(n)
      progress(n(), len)
      add(f)
    end

    list.iter(add, l)
  end

  # Refresh the library.
  def refresh() =
    log.info(
      label=id,
      "Refreshing the library..."
    )
    t = time()
    update()
    scan()
    log.info(
      label=id,
      "Refreshed the library in #{dt(t)}s."
    )
  end

  # Find all files matching given criteria.
  def find(
    ~case_sensitive=true,
    ~artist=null,
    ~artist_contains=null,
    ~artist_matches=null,
    ~album=null,
    ~genre=null,
    ~title=null,
    ~title_contains=null,
    ~filename=null,
    ~filename_contains=null,
    ~filename_matches=null,
    ~year=null,
    ~year_ge=null,
    ~year_lt=null,
    ~bpm=null,
    ~bpm_ge=null,
    ~bpm_lt=null,
    ~condition=null
  ) =
    predicates = ref([])
    def pred(p) =
      predicates := p::predicates()
    end
    if null.defined(condition) then pred(null.get(condition)) end
    def cmp(op, k, v) =
      if
        null.defined(v)
      then
        v = null.get(v)
        p =
          if
            case_sensitive
          then
            (
              "#{k} #{op} #{sqlite.escape(v)}"
            )
          else
            (
              "UPPER(#{k}) #{op} UPPER(#{sqlite.escape(v)})"
            )
          end
        pred(p)
      end
    end
    def eqf(k, v) =
      cmp("=", k, v)
    end
    def ctf(k, v) =
      if
        null.defined(v)
      then
        v = null.get(v)
        cmp("LIKE", k, "%" ^ v ^ "%")
      end
    end
    def mtf(k, v) =
      cmp("MATCHES", k, v)
    end

    eqf("artist", artist)
    ctf("artist", artist_contains)
    mtf("artist", artist_matches)
    eqf("album", album)
    eqf("genre", genre)
    eqf("title", title)
    ctf("title", title_contains)
    eqf("filename", filename)
    ctf("basename", filename_contains)
    mtf("basename", filename_matches)
    if
      null.defined(year)
    then
      year = null.get(year)
      pred("year=#{year}")
    end
    if
      null.defined(year_ge)
    then
      year_ge = null.get(year_ge)
      pred(
        "year >= #{year_ge}"
      )
    end
    if
      null.defined(year_lt)
    then
      year_lt = null.get(year_lt)
      pred(
        "year < #{year_lt}"
      )
    end
    if
      null.defined(bpm)
    then
      bpm = null.get(bpm)
      pred("bpm=#{bpm}")
    end
    if
      null.defined(bpm_ge)
    then
      bpm_ge = null.get(bpm_ge)
      pred(
        "bpm >= #{bpm_ge}"
      )
    end
    if
      null.defined(bpm_lt)
    then
      bpm_lt = null.get(bpm_lt)
      pred(
        "bpm < #{bpm_lt}"
      )
    end
    predicates =
      string.concat(
        separator=" AND ",
        predicates()
      )

    let sqlite.query (l : [{file: string}]) =
      db.select("file", table="metadata", where=predicates)
    list.map((fun (l) -> l.file), l)
  end

  t = time()
  progress =
    if
      initial_progress
    then
      fun (n, l) ->
        print(
          newline=false,
          "#{id}: updating #{n * 100 / l}%...\r"
        )
    else
      fun (_, _) -> ()
    end

  update(progress=progress)
  log.important(
    label=id,
    "Updated library in #{dt(t)}s: #{count()} entries"
  )

  t = time()
  progress =
    if
      initial_progress
    then
      fun (n, l) ->
        print(
          newline=false,
          "#{id}: scanning #{n * 100 / l}%...\r"
        )
    else
      fun (_, _) -> ()
    end

  scan(progress=progress)
  log.important(
    label=id,
    "Scanned new files in #{dt(t)}s: #{count()} entries"
  )

  if
    null.defined(refresh_time)
  then
    thread.run(
      delay=null.get(refresh_time),
      every=null.get(refresh_time),
      refresh
    )
  end

  def clear() =
    db.delete(table="metadata")
  end

  def add_directory(d) =
    directories := d::directories()
    scan()
  end

  {find=find, refresh=refresh, add_directory=add_directory, clear=clear}
end
%endif
