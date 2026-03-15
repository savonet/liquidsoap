def f() =
  r = request.queue()
  o = output.dummy(fallible=true, r)
  o.on_position(synchronous=true, position=1., fun (_, _) -> test.pass())

  r.skip()
  r.push(request.create("../media/@wav[stereo].wav"))
end

test.check(f)
