%include "../test.liq"

def f() =
  print(
    "-- 5.1 channel layout"
  )
  def five_point_one(s) =
    if false then output.file(%ffmpeg(%audio(channel_layout = 5.1)), "", s) end
  end
  five_point_one((sine() : source(audio=pcm(5.1))))
  print(
    "ok 5.1 channel layout"
  )

  print(
    "-- invalid wav samplesize"
  )
  try
    enc = %wav(samplesize = 123456)
  catch err do
    if
      err.kind != "encoder"
      or err.message !=
        "invalid sample size"
    then
      test.fail()
    end
  end
  print(
    "ok invalid wav samplesize"
  )

  print(
    "-- wav mono/stereo in same list"
  )
  try
    let eval _ =
      "l = [%wav(channels=1), %wav(channels=2)]"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok wav mono/stereo in same list"
  )

  print(
    "-- mp3/wav mismatch in same list"
  )
  try
    let eval _ =
      "l = [%mp3(stereo), %wav(channels=1)]"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok mp3/wav mismatch in same list"
  )

  print(
    "-- stereo then mono sources in same list"
  )
  try
    let eval _ =
      "l = [(blank():source(audio=pcm(stereo))), \
       (blank():source(audio=pcm(mono)))]"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok stereo then mono sources in same list"
  )

  print(
    "-- mono then stereo sources in same list"
  )
  try
    let eval _ =
      "l = [(blank():source(audio=pcm(mono))), \
       (blank():source(audio=pcm(stereo)))]"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok mono then stereo sources in same list"
  )

  print(
    "-- non-constant mp3 stereo"
  )
  try
    let eval _ = "n = false
                  ignore(%mp3(stereo=n))"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok non-constant mp3 stereo"
  )

  print(
    "-- non-constant mp3 mono"
  )
  try
    let eval _ = "n = false
                  ignore(%mp3(mono=n))"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok non-constant mp3 mono"
  )

  print(
    "-- non-constant mp3 channels"
  )
  try
    let eval _ = "n = 2
                  ignore(%mp3(channels=n))"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok non-constant mp3 channels"
  )

  print(
    "-- non-constant ffmpeg channels"
  )
  try
    let eval _ = "n = 2
                  ignore(%ffmpeg(%audio(channels=n)))"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok non-constant ffmpeg channels"
  )

  print(
    "-- non-constant ffmpeg ac"
  )
  try
    let eval _ = "n = 2
                  ignore(%ffmpeg(%audio(ac=n)))"
    test.fail()
  catch _ do
    ()
  end
  print(
    "ok non-constant ffmpeg ac"
  )

  print(
    "-- ffmpeg metadata"
  )
  ignore(%ffmpeg(metadata = [("bla", "blo"), ("title", "gni")]))
  print(
    "ok ffmpeg metadata"
  )

  test.pass()
end

test.check(f)
