# Add 3 foo queue
settings.scheduler.queues.set([
   ...settings.scheduler.queues(),
   ("foo", 3)
])

def connect_callback() =
  ignore(http.post("http://host/on_connect"))
end

# Execute inside the foo queue
thread.run(queue="foo", connect_callback)
