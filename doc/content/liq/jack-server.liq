%ifndef output.jack
let output.jack = output.dummy
%endif

# BEGIN
output.jack(server="myjackd", id="playlist", fallible=true, playlist("~/music"))


# END