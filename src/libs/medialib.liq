def medialib(directories=[])
  db = ref([])

  def scan()
    for d = list.iterator(directories) do
      for f = list.iterator(file.ls(absolute=true, recursive=true, d)) do
        log.debug("adding file: #{f}")
        m = file.metadata(f)
        m = ("filename",f) :: m
        db := (f,m) :: db()
      end
    end
  end

  def find(~artist=null(), ~artist_contains=null())
    def p(m)
      (null.defined(artist) ? m["artist"] == null.get(artist) : true)
      and
      (null.defined(artist_contains) ? string.contains(substring=null.get(artist_contains), m["artist"]) : true)
    end
    l = list.filter(fun (fm) -> p(snd(fm)), db())
    l = list.map(fst, l)
    l
  end

  scan()
  {find = find}
end
