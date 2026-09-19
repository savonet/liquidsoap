def f() =
  test.equal(settings.harbor.timeout.get_default(), 120.)
  test.equal(settings.harbor.timeout(), 120.)
  settings.harbor.timeout.set_default(10.)
  test.equal(settings.harbor.timeout.get_default(), 10.)
  test.equal(settings.harbor.timeout(), 10.)
  settings.harbor.timeout := 2.
  test.equal(settings.harbor.timeout(), 2.)
  test.equal(settings.harbor.timeout.get_default(), 10.)
  settings.harbor.timeout.set_default(null)
  test.equal(settings.harbor.timeout.get_default(), null)
  test.pass()
end

test.check(f)
