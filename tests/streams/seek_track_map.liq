# This is a regression test for GH3252

s = single("./file1.mp3")

def set_cues(s)
  def f(_)
    in = random.int(min=5, max=10)
    out = in + random.int(min=10, max=15)
    [("liq_cue_in", "#{in}"), ("liq_cue_out", "#{out}")]
  end
  metadata.map(f, s)
end

s = set_cues(s)

thread.run(delay=1., {
  let seeked = s.seek(2.)
  if seeked >= 1. then
    test.pass()
  end
})

output.dummy(s)
