# Create a new clock and assign it to a list of sources.
# @category Liquidsoap
# @param ~sync Synchronization mode. One of: `"auto"`, `"cpu"`, `"passive"` or \
#              `"none"`. Defaults to `"auto"`, which synchronizes with the CPU \
#              clock if none of the active sources are attached to their own \
#              clock (e.g. ALSA input, etc). `"cpu"` always synchronizes with \
#              the CPU clock. `"none"` removes all synchronization control.
# @param ~on_error Error callback executed when a streaming error occurs. \
#                  When passed, all streaming errors are silenced. Intended \
#                  mostly for debugging purposes.
def clock.assign_new(~sync="auto", ~id=null(), ~on_error=null(), sources) =
  c = clock.create(id=id, sync=sync, on_error=on_error)
  list.iter(fun (s) -> c.unify(s.clock), sources)
end
