s = single("../media/@mp3[stereo].mp3")

o = output.dummy(fallible=true, s)

o.on_track(
  synchronous=true,
  fun (m) ->
    if list.assoc.mem("on_air", m) then test.pass() else test.fail() end
)
