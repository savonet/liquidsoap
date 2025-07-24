files =
  process.read.lines(
    "cat " ^
      process.quote("playlist.pls")
  )
pos = ref(0)

def get_next() =
  if
    files == []
  then
    null
  else
    file = list.nth(files, pos())
    pos := pos() + 1 mod list.length(files)
    request.create(file)
  end
end

s = request.dynamic(get_next)

# END
output.dummy(fallible=true, s)
