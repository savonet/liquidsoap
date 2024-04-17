# Enable telnet server.
# @category Interaction
# @param ~port Port on which we should listen.
def server.telnet(~port=1234) =
  settings.server.telnet.port := port
  settings.server.telnet := true
end

server.register(
  namespace="runtime.gc",
  description=
    "Run a full memory collection",
  "full_major",
  fun (_) ->
    begin
      runtime.gc.full_major()
      "Done!"
    end
)

%ifdef runtime.mem_usag
server.register(
  namespace="runtime",
  description=
    "Return a description of the memory used by the process",
  "memory",
  fun (_) ->
    begin
      let {
        process_managed_memory,
        process_physical_memory,
        process_private_memory,
        process_swapped_memory
      } = runtime.memory().pretty
      "Physical memory: #{process_physical_memory}\n\
       Private memory: #{process_private_memory}\n\
       Managed memory: #{process_managed_memory}\n\
       Swapped memory: #{process_swapped_memory}"
    end
)
%endif
