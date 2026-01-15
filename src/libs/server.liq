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
  namespace="clocks",
  description="Dump a description of the current clocks and sources",
  "dump",
  fun (_) ->
    begin
      string.concat(
        separator="\n\n",
        list.map(
          fun (c) ->
            begin
              output_descriptions =
                string.concat(
                  separator=", ",
                  list.map(fun (o) -> o.id(), c.outputs())
                )
              active_source_descriptions =
                string.concat(
                  separator=", ",
                  list.map(fun (o) -> o.id(), c.active_sources())
                )
              passive_source_descriptions =
                string.concat(
                  separator=", ",
                  list.map(fun (o) -> o.id(), c.passive_sources())
                )

              "Clock #{c.id()}:\n  Total ticks: #{c.ticks()}\n  Self_sync: #{
                c.self_sync()
              }\n  Outputs: #{output_descriptions}\n  Active sources: #{
                active_source_descriptions
              }\n  Passive sources: #{passive_source_descriptions}"
            end,
          clock.active()
        )
      )
    end
)
