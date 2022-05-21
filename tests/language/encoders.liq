def f() =
  try
    enc = %wav(samplesize=123456)
  catch err do
    if err.kind != "encoder" or err.message != "invalid sample size" then
      test.fail()
    end
  end

  test.pass()
end

test.check(f)
