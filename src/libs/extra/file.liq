# Keep a record of played files. This is primarily useful to know when a song
# was last played and avoid repetitions.
# @flag extra
# @param ~duration Duration (in seconds) after which songs are forgotten. By default, songs are not forgotten which means that the playlog will contain all the songs ever played.
# @param ~hash Function to extract an identifier from the metadata. By default, the filename is used but we could return the artist to know when a song from a given artist was last played for instance.
# @param ~persistency Set a file name where the values are stored and loaded in case the script is restarted.
# @category Source / Track processing
# @method add Record that file with given metadata has been played.
# @method last How long ago a file was played (in seconds), `infinity` is returned if the song has never been played.
def playlog(~duration=infinity, ~persistency=null(), ~hash=fun(m)->m["filename"])
  l = ref([])
  # Load from persistency file
  if null.defined(persistency) then
    if file.exists(null.get(persistency)) then
      let json.parse ( parsed : [(string * float)]? ) = file.contents(null.get(persistency))
      if null.defined(parsed) then
         l := null.get(parsed)
      end
    end
  end
  # Save into persistency file
  def save()
    if null.defined(persistency) then
      data = json.stringify(l())
      file.write(data=data, null.get(persistency))
    end
  end
  # Remove too old elements
  def prune()
    if duration != infinity then
      t = time()
      l := list.assoc.filter(fun(_, tf) -> t - tf <= duration, l())
    end
  end
  # Add a new entry
  def add(m)
    prune()
    f = hash(m)
    l := (f, time()) :: l()
    save()
  end
  # Last time this entry was played
  def last(m)
    f = hash(m)
    time() - list.assoc(default = 0.-infinity, f, l())
  end
  { add=add, last=last }
end
