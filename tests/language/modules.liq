#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

module = file.temp("tmp","module")

data = '
let export foo = 123

let export gni = { foo = "bla" }
'

file.write(data=data, module)

on_shutdown(fun() -> file.unlink(module))

def f() =
  let import x = module
   ignore(x.foo + 1)
   ignore(string.length(x.gni.foo))

  let import x = module

  ignore(x.foo + 1)

  let import {foo, gni} = module
  ignore(foo + 1)
  ignore(string.length(gni.foo))

  test.pass()
end

test.check(f)
