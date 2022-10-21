# Assign a new clock to the given source (and to other time-dependent sources)
# and return the source. It is a conveniency wrapper around clock.assign_new(),
# allowing more concise scripts in some cases.
# @category Liquidsoap
# @param ~sync Synchronization mode. One of: `"auto"`, `"cpu"`, or `"none"`. Defaults to `"auto"`, which synchronizes with the CPU clock if none of the active sources are attached to their own clock (e.g. ALSA input, etc). `"cpu"` always synchronizes with the CPU clock. `"none"` removes all synchronization control.
def replaces clock(~sync="auto",~id=null(),s)
  clock.assign_new(sync=sync,id=id,[s])
  s
end

# Create a log of clock times for all the clocks initially present. The log is
# in a simple format which you can directly use with gnuplot.
# @category Liquidsoap
# @param ~delay    Delay before setting up the clock logger. This should \
#                  be used to ensure that the logger starts only after \
#                  the clocks are created.
# @param ~every    Polling interval.
# @param logfile   Path of the log file.
def clock.log(~delay=0., ~every=1., logfile)
  # Get the current clocks
  clocks = list.map(fst,clock.status())
  # Column headers
  file.write(data="# #{string.concat(separator=' ', clocks)}", logfile)
  def report()
    status = clock.status()
    status = list.map(fun (x) -> (fst(x),string(snd(x))), status)
    status = list.map(fun (c) -> status[c], clocks)
    file.write(append=true, data="#{string.concat(separator=' ', status)}", logfile)
  end
  delay = if delay <= 0. then 0. else delay end
  thread.run(delay=delay, every=every, report)
end

# Get the current time in seconds for all allocated clocks.
# @category Liquidsoap
def clock.status.seconds()
  d = frame.duration()
  list.map(fun(ct) -> (fst(ct), float(snd(ct))*d), clock.status())
end
