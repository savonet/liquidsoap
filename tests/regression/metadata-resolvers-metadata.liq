dir = file.temp_dir(cleanup=true, "metadata-resolver")

def media_protocol(~rlog:_, ~maxtime:_, arg) =
  "#{dir}/#{arg}"
end

protocol.add("media", media_protocol)

def check_metadata(m) =
  if not list.assoc.mem("foo", m) then test.fail() end
end

def check_metadata_resolver(~metadata, _) =
  check_metadata(metadata)
  []
end

decoder.metadata.add("check_metadata_resolver", check_metadata_resolver)

def f() =
  file.copy("../media/@wav[stereo].wav", "#{dir}/file.wav")

  r = request.create("annotate:foo=123:media:file.wav")
  if not request.resolve(r) then test.fail() end

  m = request.metadata(r)
  check_metadata(m)

  test.pass()
end

test.check(f)
