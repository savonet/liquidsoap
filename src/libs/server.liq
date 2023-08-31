# Enable telnet server.
# @category Interaction
# @param ~port Port on which we should listen.
def server.telnet(~port=1234) =
  settings.server.telnet.port := port
  settings.server.telnet := true
end
