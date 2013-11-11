# In LS-548, we discovered a bug in the aac+
# decoder which prevented input.http decoder 
# from detecting an EOS.

%ifnencoder %aacplus
  print("no encoder for %aacplus: TEST SKIPPED")
%endif

%ifencoder %aacplus

# Setup an aac+ output
s = sine()
h = output.harbor(%aacplus,s,mount="test",port=546742)

# Setup an input.http
o = input.http("http://localhost:546742/test")

# Setup an output which shutdowns at the second
# start
on_shutdown({ print("TEST PASSED") })
has_started = ref 0
def on_start() = 
  has_started := !has_started + 1
  if !has_started > 2 then
    shutdown ()
  end
end
output.dummy(on_start=on_start,fallible=true,o)

# Now wait a bit, stop the harbor
# and restart it..
harbor_calls = ref 0
add_timeout(5.,
  fun () -> 
   if !harbor_calls == 0 then
     source.shutdown(h)
     harbor_calls := 1
     5.
   else
     output.harbor(%aacplus,s,mount="test",port=546742)
     (-1.)
   end)

%endif
