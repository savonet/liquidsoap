def fn() =
  connected = ref(0)
  def on_connect() =
    connected := !connected + 1
    if !connected == 2 then
      test.pass()
    end
 end

 output.srt(%wav,blank(),mode="listener", on_connect=on_connect)
 output.dummy(fallible=true,input.srt(mode="caller"))
 output.dummy(fallible=true,input.srt(mode="caller"))
end

test.check(fn)
