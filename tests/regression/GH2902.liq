%include "test.liq"

def f() =
  let {uri, cmd, extname} = process_protocol_parse(".foo,bla,gni,gno:uri:uro:ura")
  test.equals(uri, "uri:uro:ura")
  test.equals(cmd, "bla,gni,gno")
  test.equals(extname, ".foo")

  test.pass()
end

test.check(f)
