def f() =
  def five_point_one(s) =
    if false then
      output.file(%ffmpeg(%audio(channel_layout=5.1)), "", s)
    end
  end
  five_point_one((sine():source(audio=pcm(5.1))))

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

  test.pass()
end

test.check(f)
