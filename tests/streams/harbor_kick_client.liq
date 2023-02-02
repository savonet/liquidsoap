# Test that a second harbor client can
# disconnect existing one if needed.
def fn() =
 kick_client = ref(fun () -> error.raise(error.assertion))
 first_client_connected = ref(false)

 def auth(_) =
   if !first_client_connected then
     fn = !kick_client
     fn()
   end

   true
 end

 def on_connect(_) =
   if !first_client_connected then
     test.pass()
   end
   first_client_connected := true
 end

 s = input.harbor(buffer=0.1, auth=auth, on_connect=on_connect, port=9878, "harbor_kick_client")

 kick_client := s.stop

 output.dummy(fallible=true,s)

 output.icecast(%mp3, password="testtest", user="testtest", mount="harbor_kick_client", port=9878, noise())

 thread.run(delay=1.,{
   output.icecast(%mp3, password="testtest", user="testtest", mount="harbor_kick_client", port=9878, noise())
 })
end

test.check(fn)
