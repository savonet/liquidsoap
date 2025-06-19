call_counts = ref([])

def test_decoder(~metadata:_, filename) =
  call_count = list.assoc(default=ref(0), filename, call_counts())

  ref.incr(call_count)

  if
    1 < call_count()
  then
    print(
      "Call count for #{filename}: #{call_count()}"
    )
    test.fail()
  end

  call_counts :=
    [...list.assoc.remove.all(filename, call_counts()), (filename, call_count)]

  []
end
decoder.metadata.add("test_decoder", test_decoder)

radio = playlist("../media")

output.dummy(radio, fallible=true)

thread.run(delay=4., fun () -> test.pass())
