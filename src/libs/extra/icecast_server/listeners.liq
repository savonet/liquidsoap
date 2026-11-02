# The harbor decision of `listener` on the mount `entry`, in the order of
# listeners.md §1; every reply carries its final header list.
# @flag hidden
def icecast.server.listeners.accept(state, entry, listener) =
  p = state.params.get()
  compatibility = state.compatibility
  header_request = {headers = listener.headers, compatibility = compatibility}
  def reply_headers(status, first) =
    icecast.server.headers.reply(
      request=header_request,
      status=status,
      [
        ...list.map(
          fun (h) ->
            {
              name = fst(h),
              value = (string.concat([snd(h)]) : string?),
              compute = null,
              status = null
            },
          first
        ),
        ...entry.headers
      ]
    )
  end

  def refuse(refusal) =
    {
      refuse =
        {
          status = refusal.status,
          reason = refusal.reason,
          headers = reply_headers(refusal.status, refusal.headers)
        },
      time_limit = null,
      preroll = null,
      headers = null,
      on_dropped = null
    }
  end

  def unserved() =
    icecast.server.decision.unserved(
      listeners=entry.stats.current(),
      metadata=icecast.server.listeners.metadata(entry)
    )
  end

  mount =
    try
      icecast.server.parameters.mount(p, entry.path)
    catch err do
      icecast.server.log.important(
        state,
        "Refusing a listener of #{entry.path}: #{err.message}"
      )
      null
    end
  max_clients = p.max_clients()
  address =
    icecast.server.addresses.resolve(
      proxy_ips=p.proxy_ips(),
      forwarded_address=p?.forwarded_address,
      {
        ip = listener.ip,
        headers = listener.headers,
        protocol = listener.protocol,
        uri = listener.uri
      }
    )
  if
    not null.defined(mount)
  then
    refuse(icecast.server.refusal(403))
  elsif
    null.defined(max_clients)
    and icecast.server.listeners.total(state) >= null.get(max_clients)
  then
    refuse(icecast.server.refusal.limit(compatibility))
  else
    mount = null.get(mount)
    request =
      icecast.server.decision.request(
        server=state.server(p.hostname()),
        compatibility=compatibility,
        address=address,
        listeners=entry.stats.current(),
        user_listeners=fun (user) -> entry.sessions.of_user(user),
        listener
      )
    def decided(~refusal, ~time_limit, ~preroll, ~exempt, ~user, ~on_release) =
      def release(record) =
        icecast.server.decision.release(on_release, record)
      end

      max_listeners = mount.max_listeners
      if
        null.defined(refusal)
      then
        refusal = null.get(refusal)
        refuse(
          icecast.server.refusal(
            reason=refusal.reason,
            headers=refusal?.headers ?? [],
            refusal.status
          )
        )
      elsif
        not exempt
        and null.defined(max_listeners)
        and entry.stats.current() >= null.get(max_listeners)
      then
        release(unserved())
        target =
          mount.fallback_when_full
            ? icecast.server.mounts.full_target(state, p, entry.path)
            : null
        if
          null.defined(target)
        then
          query = listener.query_string == "" ? "" : "?#{listener.query_string}"
          refuse(
            icecast.server.redirect(307, "#{null.get(target).path}#{query}")
          )
        else
          refuse(icecast.server.refusal.limit(compatibility))
        end
      else
        duration = mount.max_listener_duration ?? 0.
        entry.sessions.add(
          listener.id,
          user=user ?? request.user,
          address=address,
          release
        )
        {
          refuse = null,
          time_limit = time_limit ?? (duration > 0. ? duration : null),
          preroll = preroll,
          headers = reply_headers(200, entry.identity()),
          on_dropped =
            fun () ->
              begin
                if
                  null.defined(entry.sessions.remove(listener.id))
                then
                  release(unserved())
                end
              end
        }
      end
    end

    decision = mount?.listener_decision
    if
      not null.defined(decision)
    then
      decided(
        refusal=null,
        time_limit=null,
        preroll=null,
        exempt=false,
        user=null,
        on_release=null
      )
    else
      decision = null.get(decision)
      outcome =
        try
          decision(request)
        catch err do
          icecast.server.log.important(
            state,
            "Listener decision of #{entry.path} failed: #{err.message}"
          )
          null
        end
      if
        not null.defined(outcome)
      then
        refuse(icecast.server.refusal(403))
      else
        outcome = null.get(outcome)
        decided(
          refusal=outcome?.refuse,
          time_limit=outcome?.time_limit,
          preroll=outcome?.preroll,
          exempt=outcome?.exempt ?? false,
          user=outcome?.user,
          on_release=outcome?.on_release
        )
      end
    end
  end
end

# A listener's session starts: counted, then its connect event.
# @flag hidden
def icecast.server.listeners.connected(state, entry, listener) =
  entry.sessions.serve(listener)
  entry.stats.connect()
  state.counters.listener_connected()
  p = state.params.get()
  state.events.listener_connect.fire(
    bindings=p.event_bindings(),
    mount=entry.path,
    icecast.server.listeners.record(state, p, entry, listener).{
      duration = 0.,
      bytes_sent = 0
    }
  )
end

# A listener's session ends: released with this listener in the count, then
# uncounted, logged, then its disconnect event.
# @flag hidden
def icecast.server.listeners.disconnected(state, entry, listener) =
  p = state.params.get()
  record = icecast.server.listeners.record(state, p, entry, listener)
  listeners = entry.stats.current()
  session = entry.sessions.remove(listener.id)
  entry.stats.disconnect(listener)
  if
    null.defined(session)
  then
    release = null.get(session).release
    release(
      {
        duration = listener.duration,
        bytes_sent = listener.bytes_sent,
        listeners = listeners,
        metadata = icecast.server.listeners.metadata(entry)
      }
    )
  end
  state.logs.access(record)
  state.events.listener_disconnect.fire(
    bindings=p.event_bindings(),
    mount=entry.path,
    record
  )
end
