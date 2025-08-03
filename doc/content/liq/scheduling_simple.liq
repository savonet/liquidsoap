thread.run(
  every=600.,
  # This is the same as: fun () -> log("10 minutes have passed.")
  {
    log(
      "10 minutes have passed."
    )
  }
)
