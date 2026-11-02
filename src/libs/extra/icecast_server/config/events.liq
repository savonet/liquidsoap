# The `event_bindings` of an `<event-bindings>` element, or its old name
# `<kartoffelsalat>`, at the root or in a `<mount>` (icecast-config.md §9).
# A handler receives the binding record `{event, mount, listeners}`.

# The events of a trigger, each with whether it fires on that record.
# @flag hidden
def icecast.server.config.trigger_events(trigger) =
  def always(_) =
    true
  end

  if
    trigger == "source-connect"
  then
    [("source_connect", always)]
  elsif
    trigger == "source-disconnect"
  then
    [("source_disconnect", always)]
  elsif
    trigger == "format-metadata-changed"
  then
    [("metadata", always)]
  elsif
    trigger == "source-listener-attach"
  then
    [("listener_connect", always)]
  elsif
    trigger == "source-listeners-changed"
  then
    [("listener_connect", always), ("listener_disconnect", always)]
  elsif
    trigger == "source-listeners-is-zero"
  then
    [("listener_disconnect", fun (record) -> record.listeners == 0)]
  else
    []
  end
end

# Runs `executable` in the background. `argvtype` picks its arguments as
# Icecast does: none for `no_defaults`, the mount and the trigger for
# `uri_and_trigger`, the mount otherwise.
# @flag hidden
def icecast.server.config.exec_handler(~argvtype, ~trigger, executable) =
  fun (record) ->
    begin
      args =
        if
          argvtype == "no_defaults"
        then
          []
        elsif
          argvtype == "uri_and_trigger"
        then
          [record.mount, trigger]
        else
          [record.mount]
        end
      ignore(
        process.run(
          env=[...environment(), ("EVENT_TRIGGER", trigger)],
          process.quote.command(args=args, executable)
        )
      )
    end
end

# POSTs the binding record to `url`; a failure is logged.
# @flag hidden
def icecast.server.config.url_handler(~options, ~trigger, url) =
  config = icecast.server.url_auth.config("icecast-2.5", options)
  action = options?.action ?? trigger
  fun (record) ->
    try
      ignore(
        icecast.server.url_auth.send(
          config=config,
          fields=[
            ("action", action),
            ("mount", record.mount),
            ("trigger", trigger),
            ("listeners", string(record.listeners))
          ],
          {user = "", password = ""},
          url
        )
      )
    catch err do
      log.severe(
        label="icecast.server",
        "Event #{trigger} for #{record.mount}: #{url} failed: #{err.message}"
      )
    end
end

# The handler of one `<event>` for `trigger`, `null` when its type is not
# translated.
# @flag hidden
def icecast.server.config.event_handler(~kind, ~options, trigger) =
  def option(name) =
    list.assoc.nullable(name, options)
  end

  if
    kind == "exec" and null.defined(option("executable"))
  then
    icecast.server.config.exec_handler(
      argvtype=option("argvtype") ?? "default",
      trigger=trigger,
      null.get(option("executable"))
    )
  elsif
    kind == "url" and null.defined(option("url"))
  then
    icecast.server.config.url_handler(
      options={
        username = option("username"),
        password = option("password"),
        action = option("action")
      },
      trigger=trigger,
      null.get(option("url"))
    )
  elsif
    kind == "log"
  then
    level = null.map(icecast.server.config.log_level, option("level")) ?? 4
    prefix = option("prefix") ?? ""
    fun (record) ->
      log(
        label="icecast.server",
        level=level,
        "#{prefix}#{trigger} on #{record.mount}"
      )
  else
    null
  end
end

# @flag hidden
def icecast.server.config.event_bindings(~mount=null, scope) =
  warn = icecast.server.config.warn
  events =
    list.flatten(
      list.map(
        fun (tag) -> xml.children("event", xml.child(tag, scope)),
        ["event-bindings", "kartoffelsalat"]
      )
    )

  def bindings(event) =
    kind = xml.string(attribute="type", event) ?? ""
    triggers =
      list.filter(
        fun (t) -> t != "",
        list.map(
          string.trim,
          string.split(
            separator=",",
            xml.string(attribute="trigger", event) ?? ""
          )
        )
      )
    options = icecast.server.config.options(event)
    list.flatten(
      list.map(
        fun (trigger) ->
          begin
            handler =
              icecast.server.config.event_handler(
                kind=kind,
                options=options,
                trigger
              )
            fired = icecast.server.config.trigger_events(trigger)
            if
              not null.defined(handler) or list.is_empty(fired)
            then
              warn(
                "Ignoring <event type=#{string.quote(kind)} trigger=#{
                  string.quote(trigger)
                }>: not translated."
              )
              []
            else
              handler = null.get(handler)
              list.map(
                fun (e) ->
                  begin
                    let (name, fires) = e
                    {
                      event = name,
                      mount = mount,
                      synchronous = false,
                      handler =
                        fun (record) ->
                          if fires(record) then handler(record) end
                    }
                  end,
                fired
              )
            end
          end,
        triggers
      )
    )
  end

  list.flatten(
    list.map(
      fun (event) ->
        begin
          if
            not null.defined(xml.string(attribute="type", event))
            or not null.defined(xml.string(attribute="trigger", event))
          then
            warn(
              "Ignoring an <event> without type or trigger."
            )
            []
          else
            bindings(event)
          end
        end,
      events
    )
  )
end
