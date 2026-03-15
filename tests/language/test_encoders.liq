def f() =
  def five_point_one(s) =
    if false then output.file(%ffmpeg(%audio(channel_layout = 5.1)), "", s) end
  end
  five_point_one((sine() : source(audio=pcm(5.1))))

  try
    enc = %wav(samplesize = 123456)
  catch err do
    if
      err.kind != "encoder"
    or

        err.message !=
          "invalid sample size"

    then
      test.fail()
    end
  end

  try
    let eval _ =
      "l = [%wav(channels=1), %wav(channels=2)]"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ =
      "l = [%mp3(stereo), %wav(channels=1)]"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ =
      "l = [(blank():source(audio=pcm(stereo))), \
       (blank():source(audio=pcm(mono)))]"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ =
      "l = [(blank():source(audio=pcm(mono))), \
       (blank():source(audio=pcm(stereo)))]"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ = "n = false
                  ignore(%mp3(stereo=n))"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ = "n = false
                  ignore(%mp3(mono=n))"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ = "n = 2
                  ignore(%mp3(channels=n))"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ = "n = 2
                  ignore(%ffmpeg(%audio(channels=n)))"
    test.fail()
  catch _ do
    ()
  end

  try
    let eval _ = "n = 2
                  ignore(%ffmpeg(%audio(ac=n)))"
    test.fail()
  catch _ do
    ()
  end

  ignore(%ffmpeg(metadata = [("bla", "blo"), ("title", "gni")]))

  test.pass()
end

test.check(f)
