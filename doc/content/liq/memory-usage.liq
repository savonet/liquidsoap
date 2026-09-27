#!./liquidsoap
def report_memory() =
  m = runtime.memory().pretty
  log.important(
    "private=#{m.process_private_memory} ocaml_heap=#{m.process_managed_memory}"
  )
end

thread.run(every=60., report_memory)
