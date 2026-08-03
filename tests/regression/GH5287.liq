# A delay_line buffers on every clock tick but is only drained when pulled. Left
# unselected in a switch, it used to grow without bound.
enabled = ref(true)

delayed = delay_line(0.5, sine())
s = switch([({enabled()}, delayed.{track_sensitive = false}), ({true}, sine())])

frames = ref(0)
max_buffered = 1.

def check() =
  frames := frames() + 1

  # Deselect the delay line, then let it run for a while.
  if frames() == 10 then enabled := false end

  buffered = list.assoc("audio", delayed.buffered())
  if
    max_buffered < buffered
  then
    test.fail(
      "delay_line buffered #{buffered}s of audio while unselected"
    )
  end

  if frames() == 500 then test.pass() end
end

s.on_frame(synchronous=true, check)

output.dummy(s)
