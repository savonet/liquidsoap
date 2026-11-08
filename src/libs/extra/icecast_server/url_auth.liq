# Sends the `listener_remove` notification of a release in the background.
# @flag hidden
def icecast.server.url_auth.notify_release(~config, ~user, request, release) =
  if
    config.listener_remove != ""
  then
    fields = icecast.server.url_auth.remove_fields(release)
    thread.run(
      fun () ->
        try
          ignore(
            icecast.server.url_auth.post(
              config=config,
              action=config.action_remove,
              user=user,
              fields=fields,
              request,
              config.listener_remove
            )
          )
        catch err do
          log.important(
            label="icecast.server",
            "listener_remove failed for listener #{request.id} on #{
              request.mount
            }: #{err.message}"
          )
        end
    )
  end
end

# The decision outcome of `outcome` (`{refuse, time_limit, preroll, exempt,
# user}`), with the release notification of an admitted listener.
# @flag hidden
def icecast.server.url_auth.decision(~config, request, outcome) =
  refuse = outcome.refuse
  rename = outcome.user
  {
    refuse = refuse,
    time_limit = outcome.time_limit,
    preroll = outcome.preroll,
    exempt = outcome.exempt,
    user = rename,
    on_release =
      if
        null.defined(refuse) or config.listener_remove == ""
      then
        null
      else
        fun (release) ->
          icecast.server.url_auth.notify_release(
            config=config,
            user=rename ?? request.user,
            request,
            release
          )
      end
  }
end

# A decision outcome without anything from a reply.
# @flag hidden
def icecast.server.url_auth.plain(~config, ~refuse, request) =
  icecast.server.url_auth.decision(
    config=config,
    request,
    {
      refuse = refuse,
      time_limit = null,
      preroll = null,
      exempt = false,
      user = null
    }
  )
end

# The `listener_add` reply, `null` when the request failed: an error, or a
# redirect except a `Location` under `"kh"` (url-auth.md §7, §9).
# @flag hidden
def icecast.server.url_auth.ask(~config, request, url) =
  def failed(why) =
    log.severe(
      label="icecast.server",
      "listener_add failed for listener #{request.id} on #{request.mount}: #{
        why
      }"
    )
    null
  end

  try
    reply =
      icecast.server.url_auth.post(
        config=config,
        action=config.action_add,
        user=request.user,
        fields=icecast.server.url_auth.add_fields(config=config, request),
        request,
        url
      )
    kh_location =
      config.kh and null.defined(list.assoc.nullable("location", reply.headers))
    if
      reply.status >= 300 and reply.status < 400 and not kh_location
    then
      failed(
        "the backend redirected (status #{reply.status})"
      )
    else
      reply
    end
  catch err do
    failed(err.message)
  end
end

# Icecast's URL authentication as a listener decision: a `listener_add`
# request per listener, whose reply admits, refuses or redirects it, and a
# `listener_remove` notification when an admitted listener is released. A
# request that fails refuses with `401`, or admits under `presume_innocent`.
# @category Internet
# @flag extra
# @param ~cache Preroll cache loading the prerolls the backend chooses, from `icecast.server.preroll_cache`.
# @param options Icecast's options: `listener_add` (or `client_add`), `listener_remove` (or `client_remove`), `username`, `password`, `auth_header`, `timelimit_header`, `header_auth`, `header_timelimit`, `header_message`, `header_alter_action`, `header_alter_argument`, `action_add`, `action_remove`, `headers`, `header_prefix`, `timeout`, `presume_innocent` and `on_error_wait`.
def icecast.server.decision.url_auth(~cache, options) =
  failing_until = ref(0.)
  fun (request) ->
    begin
      config = icecast.server.url_auth.config(request.compatibility, options)
      asking = config.listener_add != "" and time() >= failing_until()
      reply =
        asking
          ? icecast.server.url_auth.ask(
            config=config,
            request,
            config.listener_add
          )
          : null
      if
        asking and not null.defined(reply) and config.on_error_wait > 0.
      then
        failing_until := time() + config.on_error_wait
      end
      status = null.defined(reply) ? null.get(reply).status : 0
      if
        config.listener_add == ""
        or (
          config.presume_innocent and (not null.defined(reply) or status >= 400)
        )
      then
        icecast.server.url_auth.plain(config=config, refuse=null, request)
      elsif
        not null.defined(reply)
      then
        icecast.server.url_auth.plain(
          config=config,
          refuse=icecast.server.refusal(401),
          request
        )
      else
        icecast.server.url_auth.decision(
          config=config,
          request,
          icecast.server.url_auth.outcome(
            cache=cache,
            config=config,
            request,
            null.get(reply)
          )
        )
      end
    end
end
