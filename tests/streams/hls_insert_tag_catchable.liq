%include "../test.liq"

# metadata.md §3: a rejected tag raises an `error.invalid` the script can
# catch, on the output and on a `streams()` entry alike.
dir = file.temp_dir("hls_insert_tag_catchable")
on_cleanup({file.rmdir(dir)})

s = sine()
clock.assign_new(sync="none", [s])

o = output.file.hls(segment_duration=1., dir, [("a", %mp3)], s)

checks = ref(0)

def expect_invalid(name, f) =
  kind =
    try
      f()
      "none"
    catch e do
      e.kind
    end
  if
    kind != "invalid"
  then
    test.fail(
      "#{name}: raised #{kind}, expected invalid"
    )
  end
  ref.incr(checks)
end

def check() =
  expect_invalid("o.insert_tag", {o.insert_tag("bad")})
  expect_invalid(
    "o.streams()[0].insert_tag",
    {list.hd(o.streams()).insert_tag("bad")}
  )
  expect_invalid(
    "o.insert_tag with a newline",
    {o.insert_tag("#EXT-X-A\n#EXT-X-B")}
  )
  if
    checks() != 3
  then
    test.fail(
      "#{checks()} checks ran"
    )
  end
  test.pass()
end

started = ref(false)
o.on_file_change(
  synchronous=true,
  fun (_) ->
    if
      not started()
    then
      started := true
      thread.run(check)
    end
)
