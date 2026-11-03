#!../../liquidsoap

%include "../test.liq"

def check_value(v, v') =
  if
    v != v'
  then
    print(
      "Expected: #{v}, got: #{v'}"
    )
    test.fail()
  end
end

def f() =
  before = int_of_float(time())
  local = int_of_float(time.make(time.local()))
  after = int_of_float(time())
  if
    local < before or after < local
  then
    print(
      "Expected local time between #{before} and #{after}, got: #{local}"
    )
    test.fail()
  end
  time.zone.set("Europe/Paris")
  check_value(
    time.string(time=1662367396.),
    "Monday, 05 September 2022 10:43:16"
  )

  test.pass()
end

test.check(f)
