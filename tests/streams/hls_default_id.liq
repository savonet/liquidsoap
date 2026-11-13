%include "../test.liq"

# operator.md §1: the default `id` is the operator's name, and `id=` is kept,
# checked once both outputs stream.
dir = file.temp_dir("hls_default_id")
on_cleanup({file.rmdir(dir)})

s = sine()
clock.assign_new(sync="none", [s])

default = output.file.hls(segment_duration=1., "#{dir}/a", [("a", %mp3)], s)
named =
  output.file.hls(
    id="myhls",
    segment_duration=1.,
    playlist="x.m3u8",
    "#{dir}/b",
    [("b", %mp3)],
    s
  )

checks = ref(0)

def expect_id(o, expected) =
  if
    o.id() != expected
  then
    test.fail(
      "id is #{o.id()}, expected #{expected}"
    )
  end
  ref.incr(checks)
end

def check() =
  expect_id(default, "output.file.hls")
  expect_id(named, "myhls")
  if
    checks() != 2
  then
    test.fail(
      "#{checks()} checks ran"
    )
  end
  test.pass()
end

started = ref(false)
named.on_file_change(
  synchronous=true,
  fun (_) ->
    if
      not started()
    then
      started := true
      thread.run(check)
    end
)
