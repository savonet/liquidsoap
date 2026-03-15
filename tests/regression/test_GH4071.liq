def f() =
  s = request.queue()

  s.push(request.create("https://download.samplelib.com/mp3/sample-15s.mp3"))
  s.push(request.create("https://download.samplelib.com/mp3/sample-15s.mp3"))
  s.push(request.create("https://download.samplelib.com/mp3/sample-15s.mp3"))
  s.push(request.create("https://download.samplelib.com/mp3/sample-15s.mp3"))

  test.equal(list.length(s.queue()), 4)
  test.equal(list.length(s.queue()), 4)
  test.pass()
end

test.check(f)
