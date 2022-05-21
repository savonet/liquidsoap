s = sine()

s = insert_metadata(s)

thread.run(every=1.,{
  begin
    print("Inserting metadata")
    s.insert_metadata(new_track=true, [("foo","bla")])
  end
})

s = compress.multiband(s, [
  {frequency=200., attack=10., release=30., ratio=2., threshold=-4., gain=0.},
  {frequency=20000., attack=10., release=40., ratio=2., threshold=-2., gain=-2.}
])

s.on_metadata(fun (_) -> test.pass())

output.dummy(fallible=true, s)
