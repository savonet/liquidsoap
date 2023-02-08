# Open a named UNIX socket and connect as a client.
# @param ~non_blocking Open in non-blocking mode.
# @category File
def socket.unix.client(~non_blocking=false,path) =
  s = socket.unix(domain=socket.domain.unix)
  s.non_blocking(non_blocking)
  s.connect(socket.address.unix(path))
  (s:socket).{read = s.read, write=s.write, type = s.type, close = s.close}
end

# Open a named socket and wait for a client to connect
# @param ~non_blocking Open in non-blocking mode.
# @category File
def socket.unix.listen(~non_blocking=false,path) =
  s = socket.unix(domain=socket.domain.unix)
  s.non_blocking(non_blocking)
  s.bind(socket.address.unix(path))
  s.listen(1)
  let (s', _) = s.accept()
  close = fun () ->
   begin
    s'.close()
    s.close()
   end
  (s':socket).{read = s'.read, write=s'.write, type = s'.type, close = close}
end
