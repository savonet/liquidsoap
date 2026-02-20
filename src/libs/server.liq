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
  fun (_) -> clock.dump()
)

server.register(
  namespace="clocks",
  description="Dump the source graph for all active clocks",
  "dump_sources",
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
