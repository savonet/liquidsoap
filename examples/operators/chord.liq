#!liquidsoap -v

log.stdout.set(true)

pl = on_metadata(fun (m)->log(string_of(m)),single("chord.txt"))
pl = mux_audio(pl,audio=blank())

def ch(s)
  synth.sine(midi.chord(s))
end

output.alsa(ch(pl))
