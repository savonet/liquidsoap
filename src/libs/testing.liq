# Sleep regularly, thus inducing delays in the sound production. This is mainly
# useful for emulating network delays and testing bufferization and robustness
# of scripts.
# @category Source / Testing
# @flag experimental
# @param ~every How often we should sleep (in seconds, 0 means every frame).
# @param ~delay Delay introduced (in seconds).
# @param ~delay_random Maximum amount of time randomly added to the delay (in seconds).
# @param s Source in which the delays should be introduced.
def sleeper(~every=0.5, ~delay=0.1, ~delay_random=0., s)
  last = ref(0.)
  def f()
    now = source.time(s)
    if now + every >= last() then
      last := now
      sleep(delay + random.float(max=delay_random))
    end
  end
  source.on_frame(s,f)
end
