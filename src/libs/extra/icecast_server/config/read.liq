# Helpers shared by the sections of the configuration translation
# (icecast-config.md §1, §2). A section reads its elements with the stdlib
# `xml.*` readers, whose `error.xml` `icecast.server.config.load` turns into
# `error.icecast.server.invalid_parameters`, and reports the children it does
# not translate with `warn_children`.
let icecast.server.config = ()

# Warnings emitted so far, for tests that count them per file.
# @flag hidden
let icecast.server.config.warnings = ref(0)

# @flag hidden
def icecast.server.config.warn(message) =
  warnings = icecast.server.config.warnings
  atomic(fun () -> warnings := warnings() + 1)
  log.important(label="icecast.server.config", message)
end

# @flag hidden
def icecast.server.config.reject(message) =
  error.raise(error.icecast.server.invalid_parameters, message)
end

# Warns about each child element of `node` whose tag is not in `known`, with
# the reason `ignored` gives for it, else as unknown. An `ignored` tag ending
# in `*` names every tag with that prefix. A `null` node has no children.
# @flag hidden
def icecast.server.config.warn_children(~ignored=[], ~known, node) =
  enclosing = null.case(node, {""}, fun (node) -> fst(node))
  def names(tag, entry) =
    let (name, _) = entry
    string.contains(suffix="*", name)
      ? string.contains(
        prefix=string.sub(name, start=0, length=string.length(name) - 1),
        tag
      )
      : (name == tag)
  end

  def reason(tag) =
    list.case(
      list.filter(fun (entry) -> names(tag, entry), ignored),
      "unknown element",
      fun (entry, _) -> snd(entry)
    )
  end

  list.iter(
    fun (tag) ->
      if
        not list.mem(tag, known)
      then
        icecast.server.config.warn(
          "Ignoring <#{tag}> in <#{enclosing}>: #{reason(tag)}."
        )
      end,
    xml.tags(node)
  )
end

# The Liquidsoap level of an Icecast log level (icecast-config.md §6.2), `null`
# for a value that names none.
# @flag hidden
def icecast.server.config.log_level(value) =
  level = string.case(lower=true, string.trim(value))
  if
    list.mem(level, ["1", "error"])
  then
    2
  elsif
    list.mem(level, ["2", "warn", "warning"])
  then
    3
  elsif
    list.mem(level, ["3", "info", "information"])
  then
    4
  elsif
    list.mem(level, ["4", "debug"])
  then
    5
  else
    null
  end
end

# The `<option name= value=>` children of `node` as pairs, in file order.
# @flag hidden
def icecast.server.config.options(node) =
  list.filter_map(
    fun (option) ->
      begin
        name = xml.string(attribute="name", option)
        null.defined(name)
          ? (null.get(name), xml.string(attribute="value", option) ?? "")
          : null
      end,
    xml.children("option", node)
  )
end

# The first `<option>` child of `node` named `name`, `null` when there is none.
# @flag hidden
def icecast.server.config.option(name, node) =
  list.case(
    list.filter(
      fun (option) -> xml.string(attribute="name", option) == name,
      xml.children("option", node)
    ),
    null,
    fun (option, _) -> option
  )
end
