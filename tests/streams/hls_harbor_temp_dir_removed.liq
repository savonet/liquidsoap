%include "../test.liq"

# output.harbor.hls without `tmpdir` removes its directory when it shuts down.
def hls_dirs() =
  probe = file.temp("probe", "")
  file.remove(probe)
  tmp = path.dirname(probe)
  list.filter(
    fun (f) ->
      string.sub(f, start=0, length=3) == "hls"
      and file.is_directory(path.concat(tmp, f)),
    file.ls(tmp)
  )
end

before = hls_dirs()
live =
  output.harbor.hls(
    port=18742,
    path="live",
    segment_duration=1.,
    segments=2,
    [("a", %mp3)],
    sine()
  )

created = list.filter(fun (d) -> not list.mem(d, before), hls_dirs())

def check() =
  test.equal(list.length(created), 1)
  live.shutdown()
  deadline = time() + 10.

  def poll() =
    if
      list.filter(fun (d) -> list.mem(d, created), hls_dirs()) == []
    then
      test.pass()
      (-1.)
    elsif
      time() > deadline
    then
      test.fail(
        "#{created} still there after shutdown"
      )
      (-1.)
    else
      0.2
    end
  end

  thread.run.recurrent(delay=2., poll)
end

test.check(check)
