s = sine()

reopen_count = ref(0)
filename = ref("")

def reopen_when() =
  ref.incr(reopen_count)
  count = reopen_count()

  if
    count < 2
  then
    true
  else
    fd = file.open(!filename)
    s = fd.read()
    fd.close()

    if
      string.sub(encoding="ascii", s, start=0, length=4) != "RIFF"
    then
      test.fail()
    else
      test.pass()
    end
    false
  end
end

def filename() =
  f = file.temp("tmp", ".wav")
  filename := f
  on_cleanup({file.remove(f)})
  f
end

output.file(%wav, filename, reopen_delay=1., reopen_when=reopen_when, s)
