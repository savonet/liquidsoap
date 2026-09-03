#!../../../liquidsoap
def connect() =
  ()
end

connected = ref(false)

# `exchange` writes the flag and hands back what it replaced in one step, so
# only the first caller sees `false`.
def connect_once() =
  if not connected.exchange(true) then connect() end
end

# When the others must find the work finished, not just claimed, keep the
# work inside the section.
ready = ref(false)

def prepare_once() =
  atomic(
    {
      if
        not ready()
      then
        connect()
        ready := true
      end
    }
  )
end

thread.run(delay=0., connect_once)
thread.run(delay=0., prepare_once)
