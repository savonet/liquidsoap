%include "test.liq"

set("frame.duration",0.13)
mono = (noise(duration=0.5):source(1,0,0))
stereo = (sine(duration=0.3):source(2,0,0))
variable = (rotate([(mono:source(*+1,0,0)),(stereo:source(*+1,0,0))]):source(*+1,0,0))
output.dummy(audio_to_stereo(variable))
thread.run(delay=3., { test.pass() ; shutdown() })
