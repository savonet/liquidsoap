# Create a new clock and assign it to a list of sources.
# @category Liquidsoap
# @param ~sync Synchronization mode. One of: `"auto"`, `"cpu"`, `"passive"` or `"none"`. Defaults to `"auto"`, which synchronizes with the CPU clock if none of the active sources are attached to their own clock (e.g. ALSA input, etc). `"cpu"` always synchronizes with the CPU clock. `"none"` removes all synchronization control.
def clock.assign_new(~sync="auto", ~id=null(), sources) =
  clock = clock.create(id=id, sync=sync)
  list.iter(fun (s) -> clock.unify(s.clock), sources)
end
