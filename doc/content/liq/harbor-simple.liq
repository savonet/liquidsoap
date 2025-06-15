# Custom response
def handler(req) =
  req.socket.write(
    "HTTP/1.0 201 YYR\r\nFoo: bar\r\n\r\n"
  )
  req.socket.close()

  # Null indicates that we're using the socket directly.
  null
end

harbor.http.register.simple("/custom", port=3456, handler)
