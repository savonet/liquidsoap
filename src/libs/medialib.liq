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
def medialib(~id=null(), ~persistency=null(), ~refresh=null(), ~standardize=fun(m)->m, ~initial_progress=true, ~directories=[], dir=null())
  id = string.id.default(default="medialib", id)
  refresh_time = refresh
  directories = ref(directories)
  if null.defined(dir) then directories := null.get(dir) :: directories() end

  db = ref([])

  def dt(t) = string.float(decimal_places=2,time()-t) end

  # Read metadata from file.
  def metadata(f)
    m = file.metadata.native(f)
    m = standardize(m)
    # Sanitize
    m = metadata.cover.remove(m)
    m = list.assoc.filter(fun (k,_) -> not list.mem(k, ["priv","rva2"]), m)
    # Add more metadata
    m = ("basename",path.basename(f)) :: m
    m = ("last scan",string.float(time())) :: m
    m
  end

  # Whether an entry needs ot be updated.
  def needs_update(f,m)
    file.mtime(f) > string.to_float(m["last scan"])
  end

  # Add a file to the database.
  def add(f)
    # If file doesn't exist remove it
    if not (file.exists(f)) then
      db := list.assoc.remove(f, db())
    else
      # New file or not recent enough metadata
      if not list.assoc.mem(f,db()) or needs_update(f,list.assoc(f,db())) then
        db := (f,metadata(f)) :: list.assoc.remove(f,db())
      end
    end
  end

  # Update database by renewing metadata and removing removed files.
  def update(~progress=fun(_,_)->())
    len = list.length(db())
    n = ref(0)
    nu = ref(0)
    def u(fm)
      let (f,m) = fm
      ref.incr(n)
      progress(n(),len)
      if not (file.exists(f)) then
        null()
      elsif needs_update(f,m) then
        ref.incr(nu)
        (f,metadata(f))
      else
        (f,m)
      end
    end
    db := list.filter_map(u,db())
    log.debug(label=id, "Updated #{nu()} files.")
  end

  # Make sure that new files from directories are registered.
  def scan(~progress=fun(_,_)->())
    l = list.map(fun(d) -> file.ls(absolute=true, recursive=true, d), directories())
    l = list.flatten(l)
    n = ref(0)
    len = list.length(l)
    def add(f)
      ref.incr(n)
      progress(n(),len)
      add(f)
    end
    list.iter(add, l)
  end

  # Increment when the format of the db changes
  db_version = 1

  # Load from the persistent file.
  def load()
    db := []
    if null.defined(persistency) then
      f = null.get(persistency)
      if file.exists(f) then
        try
          let json.parse ((v , parsed) : (int * [(string * [(string * string)])]?)) = file.contents(f)
          if v == db_version and null.defined(parsed) then
            db := null.get(parsed)
          end
        catch e do
          log.important(label=id,"Failed to parse persistent file #{f}: #{e.kind}: #{e.message}")
        end
      end
    end
  end

  # Store the file in a persistent file.
  def store()
    if null.defined(persistency) then
      f = null.get(persistency)
      data = json.stringify(compact=true, (db_version, db()))
      file.write(data=data, f)
      log.info(label=id,"Wrote persistent file #{f}")
    end
  end

  # Refresh the library.
  def refresh()
    log.info(label=id, "Refreshing the library...")
    t = time()
    update()
    scan()
    store()
    log.info(label=id, "Refreshed the library in #{dt(t)}s.")
  end

  # Find all files matching given criteria.
  def find(~case_sensitive=true, ~artist=null(), ~artist_contains=null(), ~artist_matches=null(), ~album=null(), ~genre=null(), ~title=null(), ~title_contains=null(), ~filename=null(), ~filename_contains=null(), ~filename_matches=null(), ~year=null(), ~year_ge=null(), ~year_lt=null(), ~bpm=null(), ~bpm_ge=null(), bpm_lt=null(), ~predicate=(fun(_)->true))
    def p(m)
      def eq(s,t) = if case_sensitive then s == t else string.case(s) == string.case(t) end end
      def contains(s, t) = if case_sensitive then string.contains(substring=s, t) else string.contains(substring=string.case(s), string.case(t)) end end
      def eqf(k,v) = null.defined(v) ? eq(m[k], null.get(v)) : true end
      def ctf(k,v) = null.defined(v) ? contains(null.get(v), m[k]) : true end
      def mtf(k,v) = null.defined(v) ? string.match(pattern=null.get(v), m[k]) : true end

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
      if null.defined(year) or null.defined(year_ge) or null.defined(year_lt) then
        if string.is_int(m["year"]) then
          y = string.to_int(m["year"])
          (null.defined(year) ? y == null.get(year) : true)
          and
          (null.defined(year_ge) ? y >= null.get(year_ge) : true)
          and
          (null.defined(year_lt) ? y < null.get(year_lt) : true)
        else
          false
        end
      else
        true
      end
      and
      if null.defined(bpm) or null.defined(bpm_ge) or null.defined(bpm_lt) then
        if string.is_int(m["bpm"]) then
          b = string.to_int(m["bpm"])
          (null.defined(bpm) ? b == null.get(bpm) : true)
          and
          (null.defined(bpm_ge) ? b >= null.get(bpm_ge) : true)
          and
          (null.defined(bpm_lt) ? b < null.get(bpm_lt) : true)
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
  log.important(label=id,"Loaded library from #{persistency} in #{dt(t)}s: #{list.length(db())} entries")
  t = time()
  progress = if initial_progress then fun(n,l) -> print(newline=false,"#{id}: updating #{n*100/l}%...\r") else fun(_,_) -> () end
  update(progress=progress)
  log.important(label=id,"Updated library in #{dt(t)}s: #{list.length(db())} entries")
  t = time()
  progress = if initial_progress then fun(n,l) -> print(newline=false,"#{id}: scanning #{n*100/l}%...\r") else fun(_,_) -> () end
  scan(progress=progress)
  log.important(label=id,"Scanned new files in #{dt(t)}s: #{list.length(db())} entries")
  store()
  log.important(label=id,"Stored library")

  if null.defined(refresh_time) then
    thread.run(delay=null.get(refresh_time), every=null.get(refresh_time), refresh)
  end

  def clear()
    db := []
  end

  def add_directory(d)
    directories := d :: directories()
    scan()
  end

  {find = find, refresh = refresh, add_directory = add_directory, clear = clear}
end
