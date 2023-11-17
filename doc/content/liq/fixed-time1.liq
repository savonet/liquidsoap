radio = switch([({ 0m-5m }, jingles), ({ true }, playlist)])
# END
output.dummy(fallible=true, radio)
