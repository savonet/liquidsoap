m = medialib("")

# BEGIN
def p(m) =
  string.length(m["artist"]) == 5
end
l = m.find(predicate=p)

# END
ignore(l)
