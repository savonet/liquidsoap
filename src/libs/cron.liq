let cron.tab = ref([])

# Add an entry to the cron tab.
# @param ~id Optional task ID.
# @param c Cron entry
# @param handler Function to execute
# @method id ID to be used to remove the task.
# @category Programming
def cron.add(~id=null, c, handler) =
  id = id ?? string.id.default(default="cron.task", null)

  if
    list.exists(fun ({id = i}) -> i == id, cron.tab())
  then
    error.raise(
      error.invalid,
      "Cron tab entry with ID #{id} already exists!"
    )
  end

  let {test} = cron.parse(c)
  log.info(
    label="cron",
    "Adding cron.tab entry #{c} (cron id: #{id})"
  )
  cron.tab := [...cron.tab(), {id=id, cron=c, test=test, handler=handler}]

  {id=id}
end

# Remove a cron tab entry. ID is returned during
# the task's registration
# @category Programming
def cron.remove(id) =
  if
    list.exists(fun ({id = i}) -> i == id, cron.tab())
  then
    log.info(
      label="cron",
      "Removing cron.tab entry #{id}"
    )
    cron.tab := list.filter(fun ({id = i}) -> i != id, cron.tab())
  else
    log.important(
      label="cron",
      "Cannot remove cron.tab entry #{id}: entry does not exist!"
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
      fun ({id, cron, test, handler}) ->
        begin
          if
            test()
          then
            log.info(
              label="cron",
              "Executing cron.tab entry #{cron} (id: #{id})"
            )
            thread.run(fast=false, handler)
          end
        end,
      cron.tab()
    )
  }
)
