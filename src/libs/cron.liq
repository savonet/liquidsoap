let cron.tab = ref([])

# Add an entry to the cron tab
# @category Programming
def cron.tab.append(entry, handler) =
  if
    list.exists(fun ({entry = e}) -> e == entry, cron.tab())
  then
    error.raise(
      error.invalid,
      "Cron tab entry for #{entry} already exists!"
    )
  end

  let {test} = cron.parse(entry)
  log.info(
    label="cron",
    "Adding cron.tab entry #{entry}"
  )
  cron.tab := [...cron.tab(), {entry=entry, test=test, handler=handler}]
end

def cron.tab.remove(entry) =
  if
    list.exists(fun ({entry = e}) -> e == entry, cron.tab())
  then
    log.info(
      label="cron",
      "Removing cron.tab entry #{entry}"
    )
    cron.tab := list.filter(fun ({entry = e}) -> e != entry, cron.tab())
  else
    log.important(
      label="cron",
      "Cannot remove cron.tab entry #{entry}: entry does not exist!"
    )
  end
end

# Main cron thread.
thread.when(
  {0m},
  {
    list.iter(
      fun ({entry, test, handler}) ->
        begin
          if
            test()
          then
            log.info(
              label="cron",
              "Executing cron.tab entry #{entry}"
            )
            handler()
          end
        end,
      cron.tab()
    )
  }
)
