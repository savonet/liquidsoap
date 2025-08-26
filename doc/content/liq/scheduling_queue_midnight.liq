request_queue = request.queue()

# BEGIN
thread.when(
  {23h59m},
  {request_queue.push(request.create("/path/to/midnight-track.mp3"))}
)

# END
output.dummy(fallible=true, request_queue)
