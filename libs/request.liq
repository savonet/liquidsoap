let request.queue = ()

let stdlib_native = native

# Play requests dynamically created by a given function.
# @category Source / Input
# @param ~id Force the value of the source ID.
# @param ~available Whether some new requests are available (when set to false, it stops after current playing request).
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~native Use native implementation.
# @param ~retry_delay Retry after a given time (in seconds) when callback returns an empty list.
# @param ~timeout Timeout (in sec.) for a single download.
# @param f Function producing requests.
def replaces request.dynamic(~id="", ~available={true}, ~retry_delay=0.1, ~conservative=false, ~default_duration=30., ~length=10., ~timeout=20., ~native=false, f) =
  if native then
    stdlib_native.request.dynamic(id=id, timeout=timeout, f).{prefetch = fun() -> (), queue_length = fun() -> 0.}
  else
    request.dynamic.list(id=id, available=available, retry_delay=retry_delay, conservative=conservative, default_duration=default_duration, length=length, timeout=timeout, {null.to_list(f())})
  end
end

# Play a queue of uris. Returns a function to push new uris in the queue as well as the resulting source.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~conservative If true, estimated remaining time on the current track is not considered when computing queue length.
# @param ~default_duration When unknown, assume this duration (in sec.) for files.
# @param ~interactive Should the queue be controllable via telnet?
# @param ~length How much audio (in sec.) should be queued in advance.
# @param ~native Use native implementation.
# @param ~queue Initial queue of requests.
# @param ~timeout Timeout (in sec.) for a single download.
# @method push Push a request on the request queue.
# @method length Length of the queue.
def request.queue(~id="", ~conservative=false, ~default_duration=30., ~interactive=true, ~length=40., ~native=false, ~queue=[], ~timeout=20.)
  queue = ref(queue)
  def next()
    reqs = !queue
    queue := []
    reqs
  end
  def push(r)
    log.info(label=id, "Pushing #{r} on the queue.")
    queue := list.append(!queue, [r])
  end
  def push_uri(uri)
    r = request.create(uri)
    push(r)
  end
  s =
    if native then
      stdlib_native.request.dynamic.list(id=id, timeout=timeout, next)
    else
      (request.dynamic.list(id=id, conservative=conservative, default_duration=default_duration, length=length, timeout=timeout, available={not list.is_empty(!queue)}, next) : source_methods)
    end
  if interactive then
    namespace = if id == "" then "queue" else id end
    def push(uri)
      r = request.create(uri)
      push(r)
      "#{request.id(r)}"
    end
    server.register(namespace=namespace, description="Push a new request in the queue.", usage="push <uri>", "push", push)
    def show_queue(_) =
      string.concat(separator=" ", list.map(fun (r) -> string_of(request.id(r)), !queue))
    end
    server.register(namespace=namespace, description="Display current queue content.", usage="queue", "queue", show_queue)
    def skip(_) = s.skip(); "Done." end
    server.register(namespace=namespace, description="Skip current song.", usage="skip", "skip", skip)
  end
  s.{push=push.{uri=push_uri}, length={list.length(!queue)}}
end

# Create a source on which plays immediately requests given with the `play`
# method.
# @category Source / Track Processing
# @method play Play a request.
# @method length Number of currently playing requests.
def request.player()
  l = ref([])

  # Perform some garbage collection in order to avoid that the list grows too
  # much.
  def collect()
    l := list.filter(remove=source.shutdown, source.is_ready, !l)
  end

  def play(r)
    collect()
    l := list.add((request.once(r):source), !l)
  end

  source.dynamic({add(normalize=false, !l)}).{play=play, length={collect(); list.length(!l)}}
end
