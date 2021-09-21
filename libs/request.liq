let request.queue = ()

let stdlib_native = native

# @docof request.dynamic.list
# @param ~native Use native implementation.
def replaces request.dynamic(%argsof(request.dynamic.list), ~native=false, f) =
  if native then
    stdlib_native.request.dynamic(%argsof(request.dynamic.list), f)
  else
    request.dynamic.list(%argsof(request.dynamic.list), {null.to_list(f())})
  end
end

# Play a queue of uris. Returns a function to push new uris in the queue as well as the resulting source.
# @category Source / Track Processing
# @param ~id Force the value of the source ID.
# @param ~interactive Should the queue be controllable via telnet?
# @param ~prefetch How many requests should be queued in advance.
# @param ~native Use native implementation.
# @param ~queue Initial queue of requests.
# @param ~timeout Timeout (in sec.) for a single download.
# @method push Push a request on the request queue.
# @method length Length of the queue.
def request.queue(~id=null(), ~interactive=true, ~prefetch=1, ~native=false, ~queue=[], ~timeout=20.)
  id = string.id.default(default="request.queue", id)
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
      request.dynamic.list(id=id, prefetch=prefetch, timeout=timeout, available={not list.is_empty(!queue)}, next)
    end
  source.set_name(s, "request.queue")
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
# @param ~simultaneous Allow multiple requests to play simultaneously. If `false` a new request replaces the previous one.
# @method play Play a request.
# @method length Number of currently playing requests.
def request.player(~simultaneous=true)
  if simultaneous then
    l = ref([])

    # Perform some garbage collection in order to avoid that the list grows too
    # much.
    def collect()
      l := list.filter(remove=source.shutdown, source.is_ready, !l)
    end

    def play(r)
      collect()
      l := request.once(r)::!l
    end

    source.dynamic({add(normalize=false, !l)}).{play=play, length={collect(); list.length(!l)}}
  else
    s = source.dynamic()
    def play(r)
      s.set(request.once(r))
    end
    s.{play=play, length={1}}
  end
end
