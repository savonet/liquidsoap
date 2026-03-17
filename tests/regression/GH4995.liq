# Regression test for GH#4995: playlist should not reload when re-used by source.dynamic

log.level.set(5)

sine_count = ref(0)

p =
  playlist(
    mode="normal",
    on_reload=fun (_) -> if sine_count() >= 1 then test.fail() end,
    "../streams/playlist"
  )

p =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("liq_test_source", "playlist")],
    p
  )

def s() =
  metadata.map(
    insert_missing=true,
    fun (_) -> [("liq_test_source", "sine")],
    sine()
  )
end

playing_sine = ref(false)

d =
  source.dynamic(
    track_sensitive=false,
    {if playing_sine() then (s() : source) else (p : source) end}
  )

playlist_count = ref(0)

d.on_metadata(
  synchronous=true,
  fun (m) ->
    begin
      log(
        "on_metadata: #{m}"
      )
      if
        m["liq_test_source"] == "playlist"
      then
        log(
          "Saw playlist metadata, switching to sine"
        )
        playing_sine := true
        ref.incr(playlist_count)

        # Double check: with no reload, files play in order!
        if
          m["filename"] != "../streams/file#{playlist_count()}.mp3"
        then
          test.fail()
        end
      elsif
        m["liq_test_source"] == "sine"
      then
        sine_count := sine_count() + 1
        log(
          "Saw sine metadata, sine_count=#{sine_count()}"
        )
        if sine_count() == 2 then test.pass() else playing_sine := false end
      end
    end
)

output.dummy(fallible=true, d)

thread.run(
  delay=8.,
  {
    print(
      "Timeout! sine_count=#{sine_count()}"
    )
    test.fail()
  }
)
