s = sine()

close_count = ref(0)

tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

output_file =  {"#{tmp_dir}/bla-#{close_count()}.mp3"}

def on_close(fname) =
  test.equals(fname, output_file())
  close_count := close_count() + 1
  if close_count() > 3 then
    test.pass()
  end
end

o = output.file(
  on_close=on_close,
  %mp3,
  output_file,
  s
)

thread.run(every=2., o.reopen)
