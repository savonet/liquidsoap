#!../../liquidsoap ../test.liq

def f() =
  try
    ignore(file.read("mqlskjdfdjnsi"))
    test.fail()
  catch _ : [error.file] do
    ()
  end

  tmpdir = file.temp_dir("testfile")
  on_cleanup({file.rmdir(tmpdir)})
  src_file = path.concat(tmpdir, "test-src-file")
  file.write(data="blablo", src_file)
  dst_file = path.concat(tmpdir, "test-dst-file")
  file.copy(src_file, dst_file)
  test.equal(file.exists(dst_file), true)
  tmpdir2 = file.temp_dir("testfile")
  on_cleanup({file.rmdir(tmpdir2)})
  file.copy(recursive=true, tmpdir, tmpdir2)
  targetdir = path.concat(tmpdir2, path.basename(tmpdir))
  test.equal(file.exists(path.concat(targetdir, "test-src-file")), true)
  non_existent = path.concat(tmpdir, "non-existent")
  dst_file2 = path.concat(tmpdir2, "another_dst")
  try
    file.copy(non_existent, dst_file2)
    test.fail()
  catch _ : [error.file] do
    ()
  end

  dst_file3 = path.concat(tmpdir, "test-dst-file3")
  file.move(src_file, dst_file3)
  test.equal(file.exists(dst_file3), true)

  file.touch("test-dst-file4")
  test.equal(file.exists("test-dst-file4"), true)

  file.mkdir(parents=true, "/tmp/a/b/c/d")
  test.equal(file.exists("/tmp/a/b/c/d"), true)

  test.pass()
end

test.check(f)
