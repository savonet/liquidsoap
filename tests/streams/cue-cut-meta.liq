def check() =
  s = playlist(prefix="annotate:liq_cue_in=0.1,foo=bla:", "playlist")

  s = cue_cut(s)

  s =
    source.on_metadata(
      s, fun (m) -> if m["foo"] == "bla" then test.pass() else test.fail() end
    )

  output.dummy(fallible=true, s)

  thread.run(delay=3., test.fail)
end

test.check(check)
