# An interpolation is read as ordinary tokens, and the `}` that closes it is
# found by counting braces over tokens. A record, a string or another
# interpolated string inside `#{ … }` therefore nests correctly.
m = [("k", "v")]
r = {a = 1}

by_key = "got #{m["k"]}"
record = "a #{ {a = 1}.a } b"
nested = "#{ "#{r.a}" }"
adjacent = "#{r.a}#{r.a}"

(by_key, record, nested, adjacent)
