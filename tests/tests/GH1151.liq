%include "test.liq"

def f() =
  if get_process_lines('echo "aa\rbb\ncc\r\ndd\r\r\nee"') !=
     ["aa\rbb", "cc", "dd\r", "ee"] then
    test.fail()
  else
    test.pass()
  end
end

thread.run(delay=0.,f)

output.dummy(blank())
