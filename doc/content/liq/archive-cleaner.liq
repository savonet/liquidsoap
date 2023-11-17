thread.run(
  every=3600.,
  {
    list.iter(
      fun (msg) -> log(msg, label="archive_cleaner"),
      list.append(
        process.read.lines(
          "find /archive/* -type f -mtime +31 -delete"
        ),
        process.read.lines(
          "find /archive/* -type d -empty -delete"
        )
      )
    )
  }
)
