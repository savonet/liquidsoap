# A library to store the metadata of files in given folders and query them. This
# is useful to generate playlists based on metadata.
# @category File
# @param ~persistency Store the database in given file, which is reuse to populate the database on next run.
# @param ~refresh Scan directories for new files every given number of seconds.
# @param ~initial_progress Show progress of library being indexed at startup.
# @param directories Directories to look for files in.
# @method find Find files according to conditions on metadata.
# @method refresh Update metadatas and look for new files.
# @method add_directory Add a new directory which should be scanned.
# @method clear Remove all known metadata.
def medialib(~id=null(), ~persistency=null(), ~refresh=null(), ~initial_progress=true, directories=[])
  id = string.id.default(default="medialib", id)
  refresh_time = refresh
  directories = ref(directories)

  db = ref([])

  def dt(t) = string.float(decimal_places=2,time()-t) end

  # Read metadata from file.
  def metadata(f)
    m = file.metadata(f)
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
      if not list.assoc.mem(f, db()) or needs_update(f,list.assoc(f,db())) then
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

  # Load from the persistent file.
  def load()
    db := []
    if null.defined(persistency) then
      f = null.get(persistency)
      if file.exists(f) then
        try
          let json.parse ( parsed : [(string * [(string * string)])]? ) = file.contents(f)
          if null.defined(parsed) then
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
      data = json.stringify(db())
      file.write(data=data, f)
      log.info(label=id,"Wrote persistent file #{f}")
    end
  end

  # Refresh the library.
  def refresh()
    t = time()
    update()
    scan()
    store()
    log.info(label=id, "Refreshed the library in #{dt(t)}s.")
  end

  # Find all files matching given criteria.
  def find(~artist=null(), ~artist_contains=null(), ~album=null(), ~genre=null(), ~title=null(), ~title_contains=null(), ~filename=null(), ~filename_contains=null(), ~filename_matches=null(), ~year=null(), ~predicate=(fun(_)->true))
    def p(m)
      myear = m["year"]

      (null.defined(artist) ? m["artist"] == null.get(artist) : true)
      and
      (null.defined(artist_contains) ? string.contains(substring=null.get(artist_contains), m["artist"]) : true)
      and
      (null.defined(album) ? m["album"] == null.get(album) : true)
      and
      (null.defined(genre) ? m["genre"] == null.get(genre) : true)
      and
      (null.defined(title) ? m["title"] == null.get(title) : true)
      and
      (null.defined(title_contains) ? string.contains(substring=null.get(title_contains), m["title"]) : true)
      and
      (null.defined(filename) ? m["filename"] == null.get(filename) : true)
      and
      (null.defined(filename_contains) ? string.contains(substring=null.get(filename_contains), m["basename"]) : true)
      and
      (null.defined(filename_matches) ? string.match(pattern=null.get(filename_matches), m["basename"]) : true)
      and
      (null.defined(year) ? (string.is_int(myear) and string.to_int(myear) == year) : true)
      and
      predicate(m)
    end
    l = list.filter(fun (fm) -> p(snd(fm)), db())
    l = list.map(fst, l)
    l
  end

  t = time()
  load()
  log.important(label=id,"Loaded library from #{persistency} in #{dt(t)}s")
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
    thread.run(every=null.get(refresh_time), refresh)
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
