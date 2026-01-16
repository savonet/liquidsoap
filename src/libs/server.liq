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
  fun (_) ->
    begin
      def rec dump_clock(~controlled_by, c) =
        c = clock(c)
        let sub_dumps =
          list.map(
            fun (c') ->
              dump_clock(
                controlled_by=" (controlled by #{c.id()})",
                c'
              ),
            c.sub_clocks()
          )
        let dumps = [c.dump(), ...sub_dumps]
        dump =
          string.concat(
            separator="\n\n",
            list.filter(fun (s) -> s != "", dumps)
          )
        "Clock #{c.id()}#{controlled_by}:\n#{dump}"
      end
      let dumps =
        list.map(fun (c) -> dump_clock(controlled_by="", c), clock.active())
      string.concat(separator="\n\n", list.filter(fun (s) -> s != "", dumps))
    end
)
