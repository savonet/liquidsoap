let json.parse v =
  '{"foo": "abc"}'

print(
  "We parsed a JSON object and got value " ^
    v.foo ^
    " for attribute foo!"
)
