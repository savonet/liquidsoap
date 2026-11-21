# Enable telnet server.
# @category Interaction
# @param ~port Port on which we should listen.
def server.telnet(~port=1234) =
  settings.server.telnet.port := port
  settings.server.telnet := true
end

server.register(
  namespace="runtime.gc",
  description="Run a full memory collection",
  "full_major",
  fun (_) ->
    begin
      runtime.gc.full_major()
      "Done!"
    end
)

server.register(
  namespace="runtime.gc",
  description="Compact OCaml memory",
  "compact",
  fun (_) ->
    begin
      runtime.gc.compact()
      "Done!"
    end
)

server.register(
  namespace="runtime",
  description="Return a description of the memory used by the process",
  "memory",
  fun (_) ->
    begin
      let {
        process_managed_memory,
        process_physical_memory,
        process_private_memory,
        process_swapped_memory
      } = runtime.memory().pretty
      "Physical memory: #{process_physical_memory}\nPrivate memory: #{
        process_private_memory
      }\nManaged memory: #{process_managed_memory}\nSwapped memory: #{
        process_swapped_memory
      }"
    end
)

server.register(
  namespace="clock",
  description="Dump a description of the current clocks and sources",
  "dump",
  fun (_) -> clock.dump()
)

server.register(
  namespace="clock",
  description="Dump the source graph for all active clocks",
  "dump_all_sources",
  fun (_) -> clock.dump_all_sources()
)

on_start(
  fun () ->
    begin
      if
        settings.request.deprecated_on_air_metadata()
      then
        server.register(
          namespace="request",
          description="Return all the requests currently on_air (DEPRECATED)!",
          "on_air",
          fun (_) ->
            string.concat(
              separator=" ",
              list.map(
                string,
                list.sort(
                  fun (a, b) -> a - b,
                  list.filter_map(
                    fun (r) ->
                      if
                        list.assoc.mem("on_air", request.metadata(r))
                      then
                        request.id(r)
                      else
                        null
                      end,
                    request.all()
                  )
                )
              )
            )
        )
      end
    end
)

let server.register.alias = ()

# The part of `s` after `prefix`, if `s` is `prefix` followed by nothing or by
# one of `separators`.
# @flag hidden
def server.register.alias.suffix(~separators=["."], prefix, s) =
  n = string.length(prefix)
  if
    s == prefix
  then
    ""
  elsif
    string.length(s) > n
    and string.sub(s, start=0, length=n) == prefix
    and list.mem(string.sub(s, start=n, length=1), separators)
  then
    string.sub(s, start=n, length=string.length(s) - n)
  else
    null
  end
end

# Register, under `alias`, the commands whose namespace `target` returns: given
# a command name, `target` returns the part of it that the alias replaces, or
# `null`. When several commands give the same aliased name, the one whose
# namespace comes first in alphabetical order is used.
# @flag hidden
def server.register.alias.custom((alias:string), target) =
  if
    alias == ""
    or string.contains(
      substring=" ",
      alias
    )
  then
    error.raise(
      error.invalid,
      "Invalid server alias #{string.quote(alias)}: it must be non-empty and \
       contain no space."
    )
  end

  suffix = server.register.alias.suffix

  # The aliased command, with the namespace it comes from.
  def aliased(command) =
    prefix = target(command.name)
    rest = null.defined(prefix) ? suffix(null.get(prefix), command.name) : null
    if
      null.defined(rest)
    then
      prefix = null.get(prefix)
      rest = null.get(rest)
      usage_rest =
        suffix(
          separators=[
            ".",
            " "
          ],
          prefix,
          command.usage
        )
      usage =
        if
          null.defined(usage_rest)
        then
          "#{alias}#{null.get(usage_rest)}"
        else
          command.usage
        end

      (
        prefix,
        {
          name = "#{alias}#{rest}",
          usage = usage,
          description = command.description,
          handler =
            fun (args) ->
              string.concat(
                separator="\r\n",
                server.execute(command.name, args)
              )
        }
      )
    else
      null
    end
  end

  def commands() =
    def compare(c, c') =
      let (prefix, _) = c
      let (prefix', _) = c'
      if prefix < prefix' then -1 elsif prefix > prefix' then 1 else 0 end
    end

    commands = list.filter_map(aliased, server.commands())
    commands = list.map(snd, list.sort(compare, commands))

    # Keep the first command for each aliased name.
    def add(l, c) =
      if list.exists(fun (c') -> c'.name == c.name, l) then l else [...l, c] end
    end

    list.fold(add, [], commands)
  end

  server.register.dynamic(commands)
end

# Make a command, or a namespace of commands, callable under another name:
# `alias` stands for `target`, and `alias.<command>` for `target.<command>`.
# The alias is resolved each time a command is called or listed, so `target`
# does not need to be registered yet, e.g. for the commands a source registers
# when it starts. A command registered under the alias name takes precedence.
# Aliases of aliases are not followed: `target` must be a registered command or
# namespace.
# @category Interaction
# @param ~target Command or namespace the alias stands for.
# @param alias Alias.
def replaces server.register.alias(~(target:string), alias) =
  if
    target == ""
  then
    error.raise(
      error.invalid,
      "Invalid server alias target: it must be non-empty."
    )
  end

  server.register.alias.custom(
    alias,
    fun (name) ->
      if
        null.defined(server.register.alias.suffix(target, name))
      then
        target
      else
        null
      end
  )
end

# Make the commands of every namespace matched by a regular expression callable
# under another name, e.g. `r/in_queue(\.\d+)?/` for a source that may get
# `in_queue`, `in_queue.1`, `in_queue.2`, etc. as id. A namespace matches when
# the leftmost match of the regular expression is the whole namespace. When
# several namespaces have a command of the same name, the one that comes first
# in alphabetical order is used (`job.10` comes before `job.2`). Like
# `server.register.alias`, it is resolved each time a command is called or
# listed.
# @category Interaction
# @param ~target Regular expression matching the namespaces the alias stands for.
# @param alias Alias.
def server.register.alias.regexp(~target, alias) =
  # The longest namespace of `name`, i.e. part before a `.`, that `target`
  # matches entirely.
  def namespace(name) =
    parts = r/\./.split(name)

    def longest(acc, part) =
      let (prefix, best) = acc
      prefix = null.case(prefix, {part}, fun (prefix) -> "#{prefix}.#{part}")
      match = target.exec(prefix)
      best =
        if
          match != [] and list.assoc(default="", 0, match) == prefix
        then
          null(prefix)
        else
          best
        end
      (null(prefix), best)
    end

    snd(list.fold(longest, (null, null), parts))
  end

  server.register.alias.custom(alias, namespace)
end
