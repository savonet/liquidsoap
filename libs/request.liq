let request.queue = ()

let stdlib_native = native

# @docof request.dynamic
# @param ~native Use native implementation.
def replaces request.dynamic(%argsof(request.dynamic), ~native=false, f) =
  if native then
    stdlib_native.request.dynamic(%argsof(request.dynamic), f)
  else
    request.dynamic(%argsof(request.dynamic), f)
  end
end

# Play a queue of requests (the first added request gets played first).
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
  fetch = ref(fun () -> true)
  def next() =
    if !queue != [] then
      let [r, ...q] = !queue
      queue := q
      (r:request)
    else
      null()
    end
  end
  def push(r)
    log.info(label=id, "Pushing #{r} on the queue.")
    queue := [...!queue, r]
    fn = !fetch
    ignore(fn())
  end
  def push_uri(uri)
    r = request.create(uri)
    push(r)
  end
  s =
    if native then
      stdlib_native.request.dynamic(id=id, timeout=timeout, next)
    else
      request.dynamic(id=id, prefetch=prefetch, timeout=timeout, available={not list.is_empty(!queue)}, next)
    end
  source.set_name(s, "request.queue")
  fetch := s.fetch
  if interactive then
    def push(uri)
      r = request.create(uri)
      push(r)
      "#{request.id(r)}"
    end
    s.on_get_ready(memoize({
      begin
        server.register(namespace=s.id(), description="Flush the queue and skip the current track",
                        "flush_and_skip", fun (_) -> try
                          s.set_queue([])
                          s.skip()
                          "Done."
                        catch err do
                          "Error while flushing and skipping source: #{err}"
                        end)
        server.register(namespace=s.id(), description="Push a new request in the queue.", usage="push <uri>", "push", push)
        def show_queue(_) =
          queue = [...s.queue(), ...(!queue)]
          string.concat(separator=" ", list.map(fun (r) -> string_of(request.id(r)), queue))
        end
        server.register(namespace=s.id(), description="Display current queue content.", usage="queue", "queue", show_queue)
        def skip(_) = s.skip(); "Done." end
        server.register(namespace=s.id(), description="Skip current song.", usage="skip", "skip", skip)
      end
    }))
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
