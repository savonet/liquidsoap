# Sleep regularly, thus inducing delays in the sound production. This is mainly
# useful for emulating network delays or sources which are slow to produce data,
# and thus test bufferization and robustness of scripts.
# @category Source / Testing
# @flag experimental
# @param ~every How often we should sleep (in seconds, 0 means every frame).
# @param ~delay Delay introduced (in seconds).
# @param ~delay_random Maximum amount of time randomly added to the delay (in seconds).
# @param s Source in which the delays should be introduced.
# @method frozen The stream production is frozen while set to `true`.
def sleeper(~every=0.5, ~delay=0.1, ~delay_random=0., s)
  last = ref(0.)
  frozen = ref(false)
  def f()
    now = source.time(s)
    while frozen() do sleep(0.1) end
    if now + every >= last() then
      last := now
      sleep(delay + random.float(max=delay_random))
    end
  end
  s = source.on_frame(s,f)
  s.{frozen = frozen}
end
