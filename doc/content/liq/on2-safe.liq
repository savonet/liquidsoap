# BEGIN
# This function turns a fallible source into an infallible source by playing a
# static single when the original song is not available.
def my_safe(s) =
  # We assume that festival is installed and functional in Liquidsoap.
  security =
    single(
      "say:Hello, this is radio FOO! We are currently having some technical \
       difficulties but we'll be back soon so stay tuned!"
    )

  # We return a fallback where the original source has priority over the
  # security single. We set track_sensitive to false to return immediately to
  # the original source when it becomes available again.
  fallback(track_sensitive=false, [s, security])
end

# END
s = sine()

# BEGIN2
s = my_safe(s)

# END2
output.dummy(fallible=true, s)
