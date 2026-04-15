%ifndef input.jack
let input.jack = blank
%endif
%ifndef output.jack
let output.jack = output.dummy
%endif
# BEGIN
# Cross from the JACK clock to another output using buffer():
output.ao(buffer(input.jack(id="input")))

# Or cross into the JACK clock from a non-JACK source:
output.jack(id="output", buffer(playlist("~/music")))
# END
