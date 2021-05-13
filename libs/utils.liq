# Get the value associated to a variable in the process environment. Returns ""
# if the variable is not set.
# @category System
def getenv(s) =
  list.assoc(default="",s,environment())
end

# Split the arguments of an url of the form `arg=bar&arg2=bar2` into
# `[("arg","bar"),("arg2","bar2")]`. The returned strings are decoded (see
# `url.decode`).
# @category String
# @param args Agument string to split.
def url.split_args(args) =
  def f(x) =
    ret = string.split(separator="=",x)
    arg = url.decode(list.nth(default="",ret,0))
    val = url.decode(list.nth(default="",ret,1))
    (arg,val)
  end
  l = string.split(separator="&",args)
  list.map(f,l)
end

# Split an url of the form `foo?arg=bar&arg2=bar2` into
# `("foo",[("arg","bar"),("arg2","bar2")])`. The returned strings are decoded
# (see `url.decode`).
# @category String
# @param uri Url to split.
def url.split(uri) =
  ret = string.extract(pattern="([^\\?]*)\\?(.*)",uri)
  args = ret[2]
  if args != "" then
    (url.decode(ret[1]),url.split_args(ret[2]))
  else
    (url.decode(uri),[])
  end
end

let frame = ()

# Duration of a frame.
# @category Liquidsoap
def frame.duration =
  setting.getter.float("frame.duration")
end

# Keep a record of played files. This is primarily useful to know when a song
# was last played and avoid repetitions.
# @param ~hash Function to extract an identifier from the metadata. By default, the filename is used but we could return the artist to know when a song from a given artist was last played for instance.
# @category Source / Track Processing
# @method add Record that file with given metadata has been played.
# @method last How long ago a file was played (in seconds), `infinity` is returned if the song has never been played.
def playlog(~hash=fun(m)->m["filename"])
  l = ref([])
  def add(m)
    f = hash(m)
    l := list.add((f, time()), !l)
  end
  def last(m)
    f = hash(m)
    time() - list.assoc(default = 0.-infinity, f, !l)
  end
  { add=add, last=last }
end
