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
# @param ~thread_queue Queue used to resolve requests.
# @param ~timeout Timeout (in sec.) for a single download.
# @method add This method is internal and should not be used. Consider using `push` instead.
# @method push Push a request on the request queue.
# @method length Length of the queue.
def request.queue(
  ~id=null(),
  ~interactive=true,
  ~prefetch=null(),
  ~native=false,
  ~queue=[],
  ~thread_queue="generic",
  ~timeout=20.
) =
  ignore(native)
  id = string.id.default(default="request.queue", id)
  initial_queue = ref(queue)
  queue = ref([])
  fetch = ref(fun () -> true)
  started = ref(false)

  def next() =
    if
      queue() != []
    then
      let [r, ...q] = queue()
      queue := q
      (r : request)
    else
      null()
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
      ignore(fn())
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
        thread_queue=thread_queue,
        timeout=timeout,
        available={not list.is_empty(queue())},
        next
      )

  s =
%ifdef native
    if
      native
    then
      stdlib_native.request.dynamic(
        id=id, thread_queue=thread_queue, timeout=timeout, next
      )
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
    fun () ->
      begin
        started := true
        s.set_queue(initial_queue())
        initial_queue := []
      end
  )

  source.set_name(s, "request.queue")
  fetch := s.fetch
  if
    interactive
  then
    def push(uri) =
      r = request.create(uri)
      push(r)
      "#{request.id(r)}"
    end

    s.on_wake_up(
      memoize(
        {
          server.register(
            namespace=source.id(s),
            description=
              "Flush the queue and skip the current track",
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

          server.register(
            namespace=source.id(s),
            description=
              "Push a new request in the queue.",
            usage=
              "push <uri>",
            "push",
            push
          )

          def show_queue(_) =
            queue = s.queue()
            string.concat(
              separator=
                " ",
              list.map(fun (r) -> string(request.id(r)), queue)
            )
          end

          server.register(
            namespace=source.id(s),
            description=
              "Display current queue content.",
            usage="queue",
            "queue",
            show_queue
          )

          def skip(_) =
            s.skip()
            "Done."
          end

          server.register(
            namespace=source.id(s),
            description=
              "Skip current song.",
            usage="skip",
            "skip",
            skip
          )
        }
      )
    )
  end

  s
end

# Play a request once and become unavailable.
# @category Source / Input
# @param ~thread_queue Queue used to resolve requests.
# @param ~timeout Timeout in seconds for resolving the request.
# @param r Request to play.
def request.once(
  ~id=null("request.once"),
  ~thread_queue="generic",
  ~timeout=20.,
  r
) =
  id = string.id.default(default="request.once", id)

  done = ref(false)
  fail = fallback([])

  def next() =
    if
      done()
    then
      null()
    else
      done := true
      if
        request.resolve(r, timeout=timeout)
      then
        request.queue(thread_queue=thread_queue, queue=[r])
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
# @param ~thread_queue Queue used to resolve requests.
# @param ~timeout Timeout (in sec.) for a single download.
# @param ~fallible Enforce fallibility of the request.
# @param r Request
def request.single(
  ~id=null("request.single"),
  ~prefetch=null(),
  ~timeout=20.,
  ~thread_queue="generic",
  ~fallible=null(),
  r
) =
  id = string.id.default(default="single", id)

  fallible = fallible ?? getter.is_constant(r)

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

  static_request = ref(null())
  done = ref(false)

  def on_wake_up() =
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
        not request.resolve(initial_request, timeout=timeout)
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
    static_request := null()
  end

  def next() =
    if
      done()
    then
      null()
    else
      done := true

      def next() =
        static_request() ?? getter.get(r)
      end

      s = request.dynamic(prefetch=prefetch, thread_queue=thread_queue, next)
      if infallible then s.set_queue([next()]) end
      s
    end
  end

  s =
    source.dynamic(
      id=id, infallible=infallible, self_sync=false, track_sensitive=true, next
    )
  s.on_wake_up(on_wake_up)
  s.on_shutdown(on_shutdown)
  (s : source_methods)
end

# Loop on a URI. It never fails if the request is static, meaning
# that it can be fetched once. Typically, http, ftp, say requests are
# static, and time is not.
# @category Source / Input
# @param ~prefetch How many requests should be queued in advance.
# @param ~timeout Timeout (in sec.) for a single download.
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

    # Perform some garbage collection in order to avoid that the list grows too
    # much.
    def collect() =
      l := list.filter(source.is_ready, l())
    end

    def play(r) =
      collect()
      l := request.once(r)::l()
    end

    source.dynamic({add(normalize=false, l())}).{
      play=play,
      length=
        {
          collect()
          list.length(l())
        }
    }
  else
    next_source = ref(null())

    def next() =
      s = next_source()
      next_source := null()
      s
    end

    s = source.dynamic(next)

    def play(r) =
      r = request.once(r)
      s.prepare(r)
      next_source := r
    end

    s.{play=play, length={1}}
  end
end
