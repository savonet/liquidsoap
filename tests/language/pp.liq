def f() =
%ifdef sine
  x = 0
%endif

  test.equal(x, 0)
%ifdef sine
  x = 1
%else
  x = 2
%endif

  test.equal(x, 1)
%ifdef xxx
  x = 3
%else
  x = 4
%endif

  test.equal(x, 4)
%ifversion >= 0.0
  x = 5
%endif

  test.equal(x, 5)
%ifversion >= 2
  x = 6
%endif

  test.equal(x, 6)
  x = 6
%ifversion < 2.0
  x = 7
%endif

%ifversion == 5145
  x = 8
%endif

  test.equal(x, 6)

  test.pass()
end

test.check(f)
