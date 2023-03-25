# Sleep regularly, thus inducing delays in the sound production. This is mainly
# useful for emulating network delays or sources which are slow to produce data,
# and thus test bufferization and robustness of scripts.
# @category Source / Testing
# @flag experimental
# @param ~every How often we should sleep (in seconds, 0 means every frame).
# @param ~delay Delay introduced (in seconds).
# @param ~delay_random Maximum amount of time randomly added to the delay (in seconds).
# @param ~on_delay Function called when a delay is introduced, with the delay as argument.
# @param s Source in which the delays should be introduced.
# @method frozen The stream production is frozen while set to `true`.
def sleeper(~every=1., ~delay=1.1, ~delay_random=0., ~on_delay=fun (_) -> (), s)
  last = ref(0.)
  frozen = ref(false)
  def f()
    while frozen() do thread.pause(0.1) end
    now = source.time(s)
    if now >= last() + every then
      last := now
      delay = delay + random.float(max=delay_random)
      on_delay(delay)
      thread.pause(delay)
    end
  end
  s = source.on_frame(s,f)
  s.{frozen = frozen}
end
