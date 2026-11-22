%include "../test.liq"

# Sources polling on a timer stop once they are shut down.
playlist_file = file.temp(cleanup=true, directory=".", "reload", ".m3u")
file.write(data="file1.mp3", playlist_file)

reloads = ref(0)
p =
  playlist(
    reload=1,
    reload_mode="seconds",
    on_reload=fun (_) -> ref.incr(reloads),
    playlist_file
  )

playlist_output = output.dummy(fallible=true, p)

fetches = ref(0)
native_source =
  native.request.dynamic(
    prefetch=1,
    retry_delay=0.2,
    fun () ->
      begin
        ref.incr(fetches)
        null
      end
  )

native_output = output.dummy(fallible=true, native_source)

def check() =
  stopped_at = ref(null)
  sources_down = ref(0)
  p.on_shutdown(synchronous=true, {ref.incr(sources_down)})
  native_source.on_shutdown(synchronous=true, {ref.incr(sources_down)})
  shutdown_requested = ref(false)

  def poll() =
    if
      null.defined(stopped_at())
    then
      let (time_stopped, reload_count, fetch_count) = null.get(stopped_at())
      if
        time() < time_stopped + 3.
      then
        0.2
      else
        test.equal(reloads(), reload_count)
        test.equal(fetches(), fetch_count)
        test.pass()
        (-1.)
      end
    elsif
      sources_down() == 2
    then
      # Counted once the sources are down: shutdown() takes them down later.
      stopped_at := (time(), reloads(), fetches())
      0.2
    elsif
      not shutdown_requested() and reloads() >= 2 and fetches() >= 2
    then
      playlist_output.shutdown()
      native_output.shutdown()
      shutdown_requested := true
      0.2
    else
      0.2
    end
  end

  thread.run.recurrent(delay=0., poll)
end

test.check(check)
