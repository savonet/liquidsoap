request_queue = request.queue()

# BEGIN
thread.run(
  every=3600.,
  {request_queue.push(request.create("/path/to/hourly-jingle.mp3"))}
)

# END
output.dummy(fallible=true, request_queue)
