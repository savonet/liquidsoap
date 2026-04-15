%ifndef input.jack
let input.jack = blank
%endif
%ifndef output.jack
let output.jack = output.dummy
%endif
# BEGIN
output.jack(id="output", input.jack(id="input"))

output.jack(
  id="playlist",
  fallible=true,
  playlist("~/music")
)
# END
