%ifdef native
let stdlib_native = native
%endif

# @docof request.dynamic
# @param ~native Use native implementation, when available.
def replaces request.dynamic(%argsof(request.dynamic), ~native=false, f) =
  ignore(native)
  default = request.dynamic(%argsof(request.dynamic), f)
%ifdef native
  if
    native
  then
    stdlib_native.request.dynamic(%argsof(request.dynamic), f)
  else
    default
  end
%else
  default
%endif
end

# Play a queue of requests (the first added request gets played first).
# @category Source / Track processing
# @param ~id Force the value of the source ID.
# @param ~interactive Should the queue be controllable via telnet?
# @param ~prefetch How many requests should be queued in advance.
# @param ~native Use native implementation, when available.
# @param ~queue Initial queue of requests.
# @param ~timeout Timeout (in sec.) to resolve the request.
# @method add This method is internal and should not be used. Consider using `push` instead.
# @method push Push a request on the request queue.
# @method length Length of the queue.
def request.queue(
  ~id=null,
  ~interactive=true,
  ~prefetch=null,
  ~native=false,
  ~queue=[],
  ~timeout=null
) =
  ignore(native)
  id = string.id.default(default="request.queue", id)
  initial_queue = ref(queue)
  queue = ref([])
  fetch = ref(fun () -> ())
  started = ref(false)

  def next() =
    if
      queue() != []
    then
      let [r, ...q] = queue()
      queue := q
      (r : request)
    else
      null
    end
  end

  def push(r) =
    if
      started()
    then
      log.info(
        label=id,
        "Pushing #{r} on the queue."
      )
      queue := [...queue(), r]
      fn = fetch()
      fn()
    else
      log.info(
        label=id,
        "Pushing #{r} on the initial queue."
      )
      initial_queue := [...initial_queue(), r]
    end
  end

  def push_uri(uri) =
    r = request.create(uri)
    push(r)
  end

  default =
    fun () ->
      request.dynamic(
        id=id,
        prefetch=prefetch,
        timeout=timeout,
        available={not list.is_empty(queue())},
        next
      )

  s =
%ifdef native
    if
      native
    then
      stdlib_native.request.dynamic(id=id, timeout=timeout, next)
    else
      default()
    end
%else
    default()
%endif

  def add(r) =
    log.severe(
      label=s.id(),
      "Please use #{s.id()}.push instead of #{s.id()}.add()"
    )

    s.add(r)
  end

  def set_queue(q) =
    if started() then queue := q else initial_queue := q end
    s.set_queue([])
  end

  def get_queue() =
    [...s.queue(), ...initial_queue(), ...queue()]
  end

  s =
    s.{
      add=add,
      push=push.{uri=push_uri},
      length={list.length(queue()) + list.length(s.queue())},
      set_queue=set_queue,
      queue=get_queue
    }

  s.on_wake_up(
    synchronous=true,
    fun () ->
      begin
        started := true
        s.set_queue(initial_queue())
        initial_queue := []
      end
  )

  fetch := s.fetch

  if
    interactive
  then
    def push(uri) =
      r = request.create(uri)
      push(r)
      "#{request.id(r)}"
    end

    s.register_command(
      description="Flush the queue and skip the current track",
      "flush_and_skip",
      fun (_) ->
        try
          s.set_queue([])
          s.skip()
          "Done."
        catch err do
          "Error while flushing and skipping source: #{err}"
        end
    )

    s.register_command(
      description="Push a new request in the queue.",
      usage="push <uri>",
      "push",
      push
    )

    def show_queue(_) =
      queue = s.queue()
      string.concat(
        separator=" ",
        list.map(fun (r) -> string(request.id(r)), queue)
      )
    end

    s.register_command(
      description="Display current queue content.",
      usage="queue",
      "queue",
      show_queue
    )

    def skip(_) =
      s.skip()
      "Done."
    end

    s.register_command(
      description="Skip current song.",
      usage="skip",
      "skip",
      skip
    )
  end

  s
end

# Play a request once and become unavailable.
# @category Source / Input
# @param ~timeout Timeout in seconds for resolving the request.
# @param r Request to play.
def request.once(~id=null("request.once"), ~timeout=null, r) =
  id = string.id.default(default="request.once", id)

  done = ref(false)
  fail = fallback([])

  def next() =
    if
      done()
    then
      null
    else
      done := true
      if
        request.resolve(r, timeout=timeout)
      then
        request.queue(queue=[r])
      else
        log.critical(
          label=id,
          "Failed to prepare track: request not ready."
        )
        request.destroy(r)
        fail
      end
    end
  end

  s = source.dynamic(self_sync=false, track_sensitive=true, next)
  (s : source_methods).{
    resolve=fun () -> request.resolve(r, timeout=timeout),
    request=r
  }
end

# Loop on a request. It never fails if the request is static, meaning
# that it can be fetched once. Typically, http, ftp, say requests are
# static, and time is not.
# @category Source / Input
# @param ~prefetch How many requests should be queued in advance.
# @param ~timeout Timeout (in sec.) to resolve the request.
# @param ~fallible Enforce fallibility of the request.
# @param r Request
def request.single(
  ~id=null("request.single"),
  ~prefetch=null,
  ~timeout=null,
  ~fallible=null,
  r
) =
  id = string.id.default(default="single", id)

  fallible = fallible ?? not getter.is_constant(r)

  infallible =
    if
      not fallible
    then
      if
        not getter.is_constant(r)
      then
        error.raise(
          error.invalid,
          "Infallible sources cannot change their underlying file."
        )
      end

      initial_request = getter.get(r)
      uri = request.uri(initial_request)
      request.is_static(uri)
    else
      false
    end

  if
    not fallible and not infallible
  then
    log.severe(
      label=id,
      "Source was marked a infallible but its request is not a static file. The \
       source is considered fallible for backward compatibility but this will \
       fail in future versions!"
    )
  end

  static_request = ref(null)

  def on_wake_up(s) =
    if
      infallible
    then
      initial_request = getter.get(r)
      uri = request.uri(initial_request)

      log.important(
        label=id,
        "#{uri} is static, resolving once for all..."
      )

      if
        not request.resolve(initial_request, timeout=timeout, content_type=s)
      then
        request.destroy(initial_request)
        error.raise(
          error.invalid,
          "Could not resolve uri: #{uri}"
        )
      else
        static_request := initial_request
      end
    end
  end

  def on_shutdown() =
    if
      null.defined(static_request())
    then
      request.destroy(null.get(static_request()))
    end
    static_request := null
  end

  def next() =
    static_request() ?? getter.get(r)
  end

  def mk_source(id) =
    request.dynamic(id=id, prefetch=prefetch, synchronous=infallible, next)
  end

  # We want to mark infallible source as such. `source.dynamic` is a nice
  # way to do it as it will raise a user-friendly error in case the underlying
  # source does not respect the conditions for being infallible.
  s =
    if
      infallible
    then
      s = mk_source("#{id}.actual")
      source.dynamic(
        id=id,
        infallible=infallible,
        self_sync=false,
        track_sensitive=true,
        {s}
      )
    else
      mk_source(id)
    end

  s.on_wake_up(synchronous=true, fun () -> on_wake_up(s))
  s.on_shutdown(synchronous=true, on_shutdown)
  (s : source_methods)
end

# Loop on a URI. It never fails if the request is static, meaning
# that it can be fetched once. Typically, http, ftp, say requests are
# static, and time is not.
# @category Source / Input
# @param ~prefetch How many requests should be queued in advance.
# @param ~timeout Timeout (in sec.) to resolve the request.
# @param ~fallible Enforce fallibility of the request.
# @param ~cue_in_metadata Metadata for cue in points. Disabled if `null`.
# @param ~cue_out_metadata Metadata for cue out points. Disabled if `null`.
# @param uri URI where to find the file
def single(
  %argsof(request.single[!id,!fallible]),
  ~id=null("single"),
  ~fallible=false,
  ~cue_in_metadata=null("liq_cue_in"),
  ~cue_out_metadata=null("liq_cue_out"),
  uri
) =
  r =
    if
      fallible
    then
      getter(
        {
          request.create(
            cue_in_metadata=cue_in_metadata,
            cue_out_metadata=cue_out_metadata,
            uri
          )
        }
      )
    else
      request.create(
        persistent=true,
        cue_in_metadata=cue_in_metadata,
        cue_out_metadata=cue_out_metadata,
        uri
      )
    end

  request.single(%argsof(request.single), r)
end

# Create a source on which plays immediately requests given with the `play`
# method.
# @category Source / Track processing
# @param ~simultaneous Allow multiple requests to play simultaneously. If `false` a new request replaces the previous one.
# @method play Play a request.
# @method length Number of currently playing requests.
def request.player(~simultaneous=true) =
  if
    simultaneous
  then
    l = ref([])
    s = ref(add(normalize=false, l()))

    # Perform some garbage collection in order to avoid that the list grows too
    # much.
    def collect(~reload_source) =
      len = list.length(l())
      l := list.filter(source.is_ready, l())
      if
        reload_source and list.length(l()) != len
      then
        s := add(normalize=false, l())
      end
    end

    def play(r) =
      collect(reload_source=false)
      l := [request.once(r), ...l()]
      s := add(normalize=false, l())
    end

    source.dynamic(s).{
      play=play,
      length=
        {
          collect(reload_source=true)
          list.length(l())
        }
    }
  else
    next_source = ref(null)

    def next() =
      s = next_source()
      next_source := null
      s
    end

    s = source.dynamic(next)

    def play(r) =
      r = request.once(r)
      r = s.prepare(r)
      next_source := r
    end

    s.{play=play, length={1}}
  end
end
