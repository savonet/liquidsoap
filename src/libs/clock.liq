# Create a new clock and assign it to a list of sources.
# @category Liquidsoap
# @param ~sync Synchronization mode. One of: `"auto"`, `"cpu"`, `"passive"` or `"none"`. Defaults to `"auto"`, which synchronizes with the CPU clock if none of the active sources are attached to their own clock (e.g. ALSA input, etc). `"cpu"` always synchronizes with the CPU clock. `"none"` removes all synchronization control.
def clock.assign_new(~sync="auto", ~id=null(), sources) =
  clock = clock.create(id=id)
  list.iter(fun (s) -> clock.unify(s.clock), sources)
  clock.start(sync)
end

# Assign a new clock to the given source (and to other time-dependent sources)
# and return the source. It is a conveniency wrapper around clock.assign_new(),
# allowing more concise scripts in some cases.
# @category Liquidsoap
# @param ~sync Synchronization mode. One of: `"auto"`, `"cpu"`, or `"none"`. Defaults to `"auto"`, which synchronizes with the CPU clock if none of the active sources are attached to their own clock (e.g. ALSA input, etc). `"cpu"` always synchronizes with the CPU clock. `"none"` removes all synchronization control.
def replaces clock(~sync="auto", ~id=null(), s) =
  clock.assign_new(sync=sync, id=id, [s])
  s
end
