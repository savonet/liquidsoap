# A library to store the metadata of files in given folders and query them. This
# is useful to generate playlists based on metadata.
# @category File
def medialib(~id=null(), ~persistency=null(), ~refresh=null(), directories=[])
  id = string.id.default(default="medialib", id)
  refresh_time = refresh

  db = ref([])

  def dt(t) = string.float(decimal_places=2,time()-t) end

  # Read metadata from file.
  def metadata(f)
    m = file.metadata(f)
    m = metadata.cover.remove(m)
    m = ("basename",path.basename(f)) :: m
    m = ("last-scan",string.float(time())) :: m
    m
  end

  # Whether an entry needs ot be updated.
  def needs_update(f,m)
    file.mtime(f) > string.to_float(m["last-scan"])
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
  def update()
    n = ref(0)
    def u(fm)
      let (f,m) = fm
      if not (file.exists(f)) then
        null()
      elsif needs_update(f,m) then
        n := n() + 1
        (f,metadata(f))
      else
        (f,m)
      end
    end
    db := list.filter_map(u,db())
    log.debug(label=id, "Updated #{n()} files.")
  end

  # Make sure that new files from directories are registered.
  def scan()
    for d = list.iterator(directories) do
      list.iter(add, file.ls(absolute=true, recursive=true, d))
    end
  end

  # Refresh the library.
  def refresh()
    t = time()
    update()
    scan()
    log.info(label=id, "Refreshed the library in #{dt(t)}s.")
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

  # Find all files matching given criteria.
  def find(~artist=null(), ~artist_contains=null(), ~genre=null(), ~title=null(), ~title_contains=null(), ~predicate=(fun(_)->true))
    def p(m)
      (null.defined(artist) ? m["artist"] == null.get(artist) : true)
      and
      (null.defined(artist_contains) ? string.contains(substring=null.get(artist_contains), m["artist"]) : true)
      and
      (null.defined(genre) ? m["genre"] == null.get(genre) : true)
      and
      (null.defined(title) ? m["title"] == null.get(title) : true)
      and
      (null.defined(title_contains) ? string.contains(substring=null.get(title_contains), m["title"]) : true)
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
  update()
  log.important(label=id,"Updated library in #{dt(t)}s: #{list.length(db())} entries")
  t = time()
  scan()
  log.important(label=id,"Scanned new files in #{dt(t)}s: #{list.length(db())} entries")
  store()
  log.important(label=id,"Stored library")

  if null.defined(refresh_time) then
    thread.run(every=null.get(refresh_time), refresh)
  end

  {find = find}
end
