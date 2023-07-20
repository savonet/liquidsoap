settings.log.level.set(4)

output.dummy(fallible = true, sine())

count = ref(0)

thread.run.recurrent({
  if count() == 5 then begin
    print("sending kill -s INT #{process.pid()}")
    process.run("kill -s INT #{process.pid()}")
    (-1.)
  end else begin
    count := count()+1
    1.
  end end
})
