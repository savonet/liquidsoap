music = playlist("~/Music")

history = ref([])
history_lock = atomic.lock()

def remember(m) =
  def push_title() =
    history := list.add(m["title"], history())
  end
  atomic(lock=history_lock, push_title)
end

music.on_metadata(synchronous=true, remember)

thread.run(
  every=60.,
  {
    print(
      string.concat(
        separator=", ",
        atomic(lock=history_lock, history)
      )
    )
  }
)
output.dummy(music)
