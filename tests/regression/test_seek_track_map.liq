# This is a regression test for GH3252
s = single("../streams/file1.mp3")

def set_cues(s) =
  def f(_) =
    in = random.int(min=5, max=10)
    out = in + random.int(min=10, max=15)
    [("liq_cue_in", "#{in}"), ("liq_cue_out", "#{out}")]
  end

  metadata.map(insert_missing=true, f, s)
end

s = set_cues(s)
thread.run(
  delay=3.,
  {
    let seeked = s.seek(2.)
    if seeked >= 1. then test.pass() else test.fail() end
  }
)

output.dummy(s)
