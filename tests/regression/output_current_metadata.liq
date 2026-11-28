%include "../test.liq"

# An output keeps the metadata in effect at the position it has reached: a
# file opened after a metadata is named after it.

dir = file.temp_dir("output_current_metadata")
on_cleanup({file.rmdir(dir)})

s = insert_metadata(sine())
clock.assign_new(sync="none", [s])

def on_close(filename) =
  print(
    "closed #{filename}"
  )
  if path.basename(filename) == "foo.wav" then test.pass() end
end

output.file(
  fallible=true,
  reopen_on_metadata=fun (_) -> true,
  on_close=on_close,
  %wav,
  "#{dir}/$(title).wav",
  s
)

thread.run(delay=0.5, {s.insert_metadata([("title", "foo")])})
thread.run(delay=1., {s.insert_metadata([("title", "bar")])})
