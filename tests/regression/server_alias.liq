%include "../test.liq"

# server.register.alias makes a command, or a namespace, callable under another
# name, resolved when called so that the target can be registered later
# (#3941).

def execute(command) =
  string.concat(separator="\n", server.execute(command))
end

# Declared before any of their targets exist.
server.register.alias(target="some.long.namespace", "short")
server.register.alias(target="some.long.namespace.hello", "hi")
server.register.alias(target="raw_queue", "q")

server.register(
  namespace="some.long.namespace",
  usage="hello <name>",
  description="Say hello.",
  "hello",
  fun (name) ->
    "hello #{name}"
)

# A command registered under the alias name takes precedence.
server.register(namespace="short", "own", fun (_) -> "own")
server.register(namespace="some.long.namespace", "own", fun (_) -> "target")

# A command with an empty namespace, matched by a regexp alias, must not break
# other aliases.
server.register(namespace="", "x", fun (_) -> "x")
server.register.alias.regexp(target=r/x/, "rx")
server.register.alias.regexp(target=r/zzz/, "rz")

# A failing provider, and one that runs commands, must not break aliases.
server.register.dynamic(fun () -> error.raise(error.not_found, "boom"))
server.register.dynamic(
  fun () ->
    begin
      ignore(server.execute("nope"))
      []
    end
)

empty_rejected =
  try
    server.register.alias(target="uptime", "")
    false
  catch _ : [error.invalid] do
    true
  end

# Commands that only exist once the source starts.
s = request.queue(id="raw_queue")
output.dummy(fallible=true, s)

# Sources whose ids get a numbered suffix. With several matching namespaces,
# the first one in alphabetical order is used.
server.register.alias.regexp(target=r/job(\.\d+)?/, "jb")
server.register(namespace="job.1", "who", fun (_) -> "job.1")
server.register(namespace="job", "who", fun (_) -> "job")
server.register.alias.regexp(target=r/worker\.\d+/, "numbered")
w1 = request.queue(id="worker")
w2 = request.queue(id="worker")
output.dummy(fallible=true, w1)
output.dummy(fallible=true, w2)

def check() =
  help = server.execute("help")
  checks =
    [
      (
        "namespace",
        execute(
          "short.hello bob"
        ) ==
          "hello bob"
      ),
      (
        "command",
        execute(
          "hi alice"
        ) ==
          "hello alice"
      ),
      ("precedence", execute("short.own") == "own"),
      (
        "help usage",
        execute(
          "help short.hello"
        ) ==
          "Usage: short.hello <name>\n  Say hello."
      ),
      (
        "listing",
        list.mem(
          "├─ short.hello <name>",
          help
        )
      ),
      (
        "source commands",
        list.exists(
          fun (l) ->
            l ==
              "├─ q.push <uri>",
          help
        )
      ),
      (
        "source exec",
        execute("q.queue") == execute("raw_queue.queue")
      ),
      ("unknown", string.contains(prefix="ERROR", execute("short.nope"))),
      (
        "empty alias",
        empty_rejected
      ),
      (
        "regexp no match",
        string.contains(prefix="ERROR", execute("rz.x"))
      ),
      (
        "help listing",
        list.mem(
          "Available commands:",
          help
        )
      ),
      (
        "regexp ids",
        w1.id() == "worker" and w2.id() == "worker.1"
      ),
      (
        "regexp first",
        execute("jb.who") == "job"
      ),
      (
        "regexp numbered",
        execute(
          "help numbered.push"
        ) ==
          "Usage: numbered.push <uri>\n  Push a new request in the queue."
      ),
      (
        "regexp exec",
        execute("numbered.queue") == execute("worker.1.queue")
      ),
      (
        "regexp listing",
        list.mem(
          "├─ numbered.push <uri>",
          help
        )
        and not list.exists(
          fun (l) ->
            l ==
              "├─ numbered.1.push <uri>",
          help
        )
      )
    ]

  failed = list.filter(fun (c) -> not snd(c), checks)
  print(
    "failed checks: #{list.map(fst, failed)}"
  )
  if failed == [] then test.pass() else test.fail() end
end

thread.run(delay=1., check)
