normal = sine()

# BEGIN
stripped_stream = blank.strip(input.http("http://myicecast:8080/live.ogg"))

s = fallback(track_sensitive=false, [stripped_stream, blank.strip(normal)])
# END

output.dummy(fallible=true, s)
