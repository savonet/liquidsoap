clock.autostart := false

def f() =
  # Test strictness and evaluation order
  l = ref(false)
  r = ref(false)
  if

      begin
        l := true
        true
      end

  or

      begin
        r := true
        true
      end

  then
    ()
  end

  test.equals(l() and (not r()), true)
  ignore(true == false ? 5 : 6 )
  test.pass()
end

test.check(f)
