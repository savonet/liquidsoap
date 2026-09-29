%include "../test.liq"

# The feed of a client kicked to make room for another must not disconnect the
# client that replaced it.
def fn() =
  connects = ref(0)
  disconnects = ref(0)
  kick = ref(fun () -> ())

  def auth(_) =
    if
      connects() == 1
    then
      fn = kick()
      fn()
    end

    true
  end

  s =
    input.harbor(
      buffer=0.1,
      auth=auth,
      port=9879,
      "harbor_kick_replaced_client"
    )

  kick := s.stop

  def check() =
    if
      disconnects() == 1 and null.defined(s.connected_client())
    then
      test.pass()
    else
      test.fail(
        "#{disconnects()} disconnections, client connected: #{
          s.connected_client()
        }"
      )
    end
  end

  def second_client() =
    ignore(
      output.icecast(
        %mp3,
        password="testtest",
        user="testtest",
        mount="harbor_kick_replaced_client",
        port=9879,
        noise()
      )
    )
  end

  s.on_disconnect(synchronous=true, fun () -> disconnects := disconnects() + 1)
  s.on_connect(
    synchronous=true,
    fun (_) ->
      begin
        connects := connects() + 1
        if
          connects() == 1
        then
          thread.run(delay=1., second_client)
        else
          thread.run(delay=3., check)
        end
      end
  )

  output.dummy(fallible=true, s)
  output.icecast(
    %mp3,
    password="testtest",
    user="testtest",
    mount="harbor_kick_replaced_client",
    port=9879,
    noise()
  )
end

test.check(fn)
