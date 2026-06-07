%ifndef input.jack
let input.jack = blank
%endif
# BEGIN
output.file(
  %wav,
  "/tmp/jack-recording.wav",
  input.jack(id="input")
)
# END
