def f() =
  test.equal(socket.internet_address.ipv6.any.is_ipv6, true)
  test.equal(socket.internet_address.ipv6.loopback.to_string(), "::1")

  let [h] = null.get(host.of_name("localhost")).addresses
  test.equal(socket.address.internet_address(h, 80).port, 80)

  fname = file.temp("unix", "socket")
  file.remove(fname)

  thread.run(fun () -> begin
    s = socket.unix.listen(fname)
    test.equal(s.type, "unix")
    s.write.wait(fun () -> begin
      s.write("done!")
      s.close()
    end)
  end)

  thread.run(delay=0.1, fun () -> begin
    s = socket.unix.client(fname)
    s.read.wait(fun () ->
     begin
      if s.read() == "done!" then
        s.close()
        test.pass()
      else
        test.fail()
      end
     end)
  end)
end

test.check(f)
