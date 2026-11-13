#!../../liquidsoap

%include "../test.liq"

def f() =
  tmpdir = file.temp_dir("testfilereadlength")
  on_cleanup({file.rmdir(tmpdir)})
  fname = path.concat(tmpdir, "data")
  data = string.concat(list.init(20000, fun (_) -> "0123456789"))
  file.write(data=data, fname)

  fd = file.open(write=false, fname)
  first = string.bytes.length(fd.read())
  if
    first != 65536
  then
    test.fail(
      "default read length: #{first}"
    )
  end
  small = string.bytes.length(fd.read(length=1000))
  if
    small != 1000
  then
    test.fail(
      "read(length=1000) returned #{small}"
    )
  end
  try
    ignore(fd.read(length=0))
    test.fail(
      "read(length=0) did not raise"
    )
  catch _ : [error.socket] do
    ()
  end
  fd.close()

  contents = file.contents(fname)
  if
    contents != data
  then
    test.fail(
      "file.contents returned #{string.bytes.length(contents)} bytes, expected #{
        string.bytes.length(data)
      }"
    )
  end

  test.pass()
end

test.check(f)
