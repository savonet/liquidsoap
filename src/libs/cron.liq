let cron.tab = ref([])

# Add an entry to the cron tab
# @param ~label Entry label
# @param ~cron Cron entry
# @category Programming
def cron.tab.append(~label, ~cron:c, handler) =
  if
    label
    list.exists(fun ({label = l}) -> l == label, cron.tab())
  then
    error.raise(
      error.invalid,
      "Cron tab entry for #{label} already exists!"
    )
  end

  let {test} = cron.parse(c)
  log.info(
    label="cron",
    "Adding cron.tab entry #{label} (cron entry: #{c})"
  )
  cron.tab := [...cron.tab(), {label=label, cron=c, test=test, handler=handler}]
end

def cron.tab.remove(label) =
  if
    list.exists(fun ({label = l}) -> l == label, cron.tab())
  then
    log.info(
      label="cron",
      "Removing cron.tab entry #{label}"
    )
    cron.tab := list.filter(fun ({label = l}) -> l != label, cron.tab())
  else
    log.important(
      label="cron",
      "Cannot remove cron.tab entry #{label}: entry does not exist!"
    )
  end
end

# Main cron thread.
thread.when(
  fast=false,
  every=20.,
  {0s-30s},
  {
    list.iter(
      fun ({label, cron, test, handler}) ->
        begin
          if
            test()
          then
            log.info(
              label="cron",
              "Executing cron.tab entry #{label} (cron entry: #{cron})"
            )
            thread.run(fast=false, handler)
          end
        end,
      cron.tab()
    )
  }
)
