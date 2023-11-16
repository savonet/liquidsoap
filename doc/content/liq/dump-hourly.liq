s = sine()

# BEGIN
# A source to dump
# s = ...

# Dump the stream
output.file(
  %wav,
  {time.string("/archive/%Y-%m-%d/%Y-%m-%d-%H_%M_%S.mp3")},
  s,
  reopen_when={}
)
