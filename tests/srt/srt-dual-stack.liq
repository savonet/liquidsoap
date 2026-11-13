# A caller with default parameters reaches a listener with default parameters
# under each address preference (spec/library.md §5).
%include "../test.liq"
%include "helpers.inc"

def pair(~port, ~prefer_address) =
  i = input.srt(port=port, prefer_address=prefer_address)
  output.dummy(fallible=true, i)
  output.srt(port=port, prefer_address=prefer_address, %wav, sine())
  i
end

pairs =
  [
    ("system", pair(port=19552, prefer_address=null)),
    ("ipv4", pair(port=19554, prefer_address="ipv4")),
    ("ipv6", pair(port=19556, prefer_address="ipv6"))
  ]

def run() =
  list.iter(
    fun (entry) ->
      begin
        let (name, i) = entry
        wait_until(
          timeout=20.,
          what="#{name}: a connection",
          {i.is_ready()}
        )
      end,
    pairs
  )
  test.pass()
end

thread.run(delay=0.1, run)
