let request.queue = ()

%ifdef native
let stdlib_native = native
%endif

# @docof request.dynamic
# @param ~native Use native implementation, when available.
def replaces request.dynamic(%argsof(request.dynamic), ~native=false, f) =
  ignore(native)
%ifdef native
  if native then
    stdlib_native.request.dynamic(%argsof(request.dynamic), f)
  else
%else
  begin
%endif
    request.dynamic(%argsof(request.dynamic), f)
  end
end

# Play a queue of requests (the first added request gets played first).
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~interactive Should the queue be controllable via telnet?
# @param ~prefetch How many requests should be queued in advance.
# @param ~native Use native implementation, when available.
# @param ~queue Initial queue of requests.
# @param ~timeout Timeout (in sec.) for a single download.
# @method add This method is internal and should not be used. Consider using `push` instead.
# @method push Push a request on the request queue.
# @method length Length of the queue.
def request.queue(~id=null(), ~interactive=true, ~prefetch=1, ~native=false, ~queue=[], ~timeout=20.)
  id = string.id.default(default="request.queue", id)
  ignore(native)
  queue = ref(queue)
  fetch = ref(fun () -> true)
  def next() =
    if queue() != [] then
      let [r, ...q] = queue()
      queue := q
      (r:request)
    else
      null()
    end
  end
  def push(r)
    log.info(label=id, "Pushing #{r} on the queue.")
    queue := [...queue(), r]
    fn = fetch()
    ignore(fn())
  end
  def push_uri(uri)
    r = request.create(uri)
    push(r)
  end
  s =
%ifdef native
    if native then
      stdlib_native.request.dynamic(id=id, timeout=timeout, next)
    else
%else
    begin
%endif
      request.dynamic(id=id, prefetch=prefetch, timeout=timeout, available={not list.is_empty(queue())}, next)
    end
  source.set_name(s, "request.queue")
  fetch := s.fetch
  if interactive then
    def push(uri)
      r = request.create(uri)
      push(r)
      "#{request.id(r)}"
    end
    s.on_wake_up(memoize({
      begin
        server.register(namespace=source.id(s), description="Flush the queue and skip the current track",
                        "flush_and_skip", fun (_) -> try
                          s.set_queue([])
                          s.skip()
                          "Done."
                        catch err do
                          "Error while flushing and skipping source: #{err}"
                        end)
        server.register(namespace=source.id(s), description="Push a new request in the queue.", usage="push <uri>", "push", push)
        def show_queue(_) =
          queue = [...s.queue(), ...(queue())]
          string.concat(separator=" ", list.map(fun (r) -> string(request.id(r)), queue))
        end
        server.register(namespace=source.id(s), description="Display current queue content.", usage="queue", "queue", show_queue)
        def skip(_) = s.skip(); "Done." end
        server.register(namespace=source.id(s), description="Skip current song.", usage="skip", "skip", skip)
      end
    }))
  end

  def add(r) =
    log.severe(label=s.id(), "Please use #{s.id()}.push instead of #{s.id()}.add()")
    s.add(r)
  end

  def set_queue(q)
    queue := q
    s.set_queue([])
  end

  s.{
    add=add,
    push=push.{uri=push_uri},
    length={list.length(queue())+list.length(s.queue())},
    set_queue=set_queue
  }
end

# Create a source on which plays immediately requests given with the `play`
# method.
# @category Source / Track processing
# @param ~simultaneous Allow multiple requests to play simultaneously. If `false` a new request replaces the previous one.
# @method play Play a request.
# @method length Number of currently playing requests.
def request.player(~simultaneous=true)
  if simultaneous then
    l = ref([])

    # Perform some garbage collection in order to avoid that the list grows too
    # much.
    def collect()
      l := list.filter(source.is_ready, l())
    end

    def play(r)
      collect()
      l := request.once(r)::l()
    end

    source.dynamic({add(normalize=false, l())}).{play=play, length={collect(); list.length(l())}}
  else
    s = source.dynamic()
    def play(r)
      s.set(request.once(r))
    end
    s.{play=play, length={1}}
  end
end
