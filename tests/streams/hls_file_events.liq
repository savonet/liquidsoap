%include "../test.liq"

# files.md §8, known-complexity.md KC13: per file one `created`, then any
# `updated`, then at most one `deleted`, with the same path, the file complete
# and present at each; no event for temporary or state files.
dir = file.temp_dir("hls_file_events")
on_cleanup({file.rmdir(dir)})
out = "#{dir}/out"
segments_wanted = 20

s = sine()
clock.assign_new(sync="none", [s])

def fail_on_error(err) =
  test.fail(
    "error while streaming: #{err}"
  )
  null
end

published = ref(["a.m3u8", "b.m3u8", "stream.m3u8"])

def segment_name(segment) =
  name = "#{segment.stream_name}_#{segment.position}.#{segment.extname}"
  published := name::published()
  name
end

history = ref([])
created_count = ref(0)
updated_count = ref(0)
deleted_count = ref(0)

def check_complete(event) =
  if
    not file.exists(event.path)
  then
    test.fail(
      "#{event.state} for #{event.path}, which does not exist"
    )
  end
  data = file.contents(event.path)
  if
    string.contains(suffix=".m3u8", event.path)
  then
    if
      not string.contains(prefix="#EXTM3U\r\n", data)
      or not string.contains(suffix="\r\n", data)
    then
      test.fail(
        "#{event.state} for an incomplete playlist #{event.path}"
      )
    end
  elsif
    string.sub(encoding="ascii", data, start=0, length=3) != "ID3"
    or string.bytes.length(data) <= 80
  then
    test.fail(
      "#{event.state} for an incomplete segment #{event.path}"
    )
  end
end

def on_change(event) =
  name =
    string.sub(
      event.path,
      start=string.length(out) + 1,
      length=string.length(event.path) - string.length(out) - 1
    )

  if
    not string.contains(prefix="#{out}/", event.path)
    or not list.mem(name, published())
  then
    test.fail(
      "event #{event.state} for #{event.path}, not a file the output publishes"
    )
  end
  past = list.assoc(default=[], event.path, history())
  if
    list.mem("deleted", past)
  then
    test.fail(
      "#{event.state} for #{event.path} after its deleted event"
    )
  end
  if
    past == [] and event.state != "created"
  then
    test.fail(
      "first event for #{event.path} is #{event.state}"
    )
  end
  if
    past != [] and event.state == "created"
  then
    test.fail(
      "second created event for #{event.path}"
    )
  end
  check_complete(event)
  history := list.assoc.remove(event.path, history())
  history := (event.path, [...past, event.state])::history()
  if event.state == "created" then created_count := created_count() + 1 end
  if event.state == "updated" then updated_count := updated_count() + 1 end
  if event.state == "deleted" then deleted_count := deleted_count() + 1 end
  if
    not test.done() and event.path == "#{out}/b_#{segments_wanted}.mp3"
  then
    if
      created_count() < 2 * segments_wanted
      or updated_count() == 0
      or deleted_count() == 0
    then
      test.fail(
        "too few events: #{created_count()} created, #{updated_count()} updated, \
         #{deleted_count()} deleted"
      )
    end
    if
      not file.exists("#{out}/state.json")
    then
      test.fail(
        "no state file written, so its events were not exercised"
      )
    end
    print(
      "#{created_count()} created, #{updated_count()} updated, #{
        deleted_count()
      } deleted events checked"
    )
    test.pass()
  end
end

o =
  output.file.hls(
    reopen_on_error=fail_on_error,
    segment_duration=1.,
    segments=3,
    segments_overhead=1,
    persist_at="state.json",
    segment_name=segment_name,
    out,
    [("a", %mp3), ("b", %shine(bitrate = 64))],
    s
  )

o.on_file_change(synchronous=true, on_change)
