cron.add(
  id="minight-task",
  "0 0 * * *",
  {
    log(
      "Midnight event"
    )
  }
)
