def medialib(~id=null(), ~persistency=null(), directories=[])
  id = string.id.default(default="medialib", id)

  db = ref([])

  # Read metadata from file
  def metadata(f)
    m = file.metadata(f)
    m = metadata.cover.remove(m)
    m = ("filename",f) :: m
    m = ("scan-time",string.float(time())) :: m
    m
  end

  # Add a file to the database.
  def add(f)
    # If file doesn't exist remove it
    if not (file.exists(f)) then
      db := list.assoc.remove(f, db())
    else
      # Not recent enough metadata
      if not list.assoc.mem(f, db()) or (string.to_float(list.assoc("scan-time",list.assoc(f, db()))) > file.mtime(f)) then
        db := (f,metadata(f)) :: list.assoc.remove(f, db())
      end
    end
  end

  # Update database by renewing metadata and removing removed files.
  def update()
    for fm = list.iterator(db()) do
      let (f,_) = fm
      add(f)
    end
  end

  # Make sure that new files from directories are registered.
  def scan()
    for d = list.iterator(directories) do
      list.iter(add, file.ls(absolute=true, recursive=true, d))
    end
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
          log.important(label=id,"Failed to parse persistent file #{f}")
          log.important(label=id,"#{e.kind}: #{e.message}")
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

  def find(~artist=null(), ~artist_contains=null(), ~genre=null(), ~title=null(), ~title_contains=null(), ~predicate=(fun(m)->true))
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

  load()
  scan()
  store()

  {find = find}
end
