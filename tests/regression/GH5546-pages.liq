%include "../test.liq"

# Regression for https://github.com/savonet/liquidsoap/issues/5546: with a
# burst spanning several Ogg links, a late listener starts at a link's header
# and never receives a page of an earlier link. Decoders skip such stray pages,
# so the bytes are checked directly.
port = 3547
s = insert_metadata(sine())
links = ref(0)

thread.run(
  every=1.,
  fun () ->
    begin
      ref.incr(links)
      s.insert_metadata(
        [
          (
            "title",
            "link #{links()}"
          )
        ]
      )
    end
)

output.harbor(
  port=port,
  mount="ffmpeg",
  burst=4096,
  %ffmpeg(format = "ogg", %audio(codec = "libopus", samplerate = 48000)),
  s
)

def byte(data, pos) =
  string.binary.to_int(string.sub(encoding="ascii", data, start=pos, length=1))
end

def serial(data, pos) =
  string.binary.to_int(
    string.sub(encoding="ascii", data, start=pos + 14, length=4)
  )
end

# Pages of [data] as [(begins_link, serial)], the last partial page excluded.
def pages(mount, data) =
  len = string.length(encoding="ascii", data)
  def rec parse(pos, acc) =
    if
      pos + 27 > len
    then
      list.rev(acc)
    else
      if
        string.sub(encoding="ascii", data, start=pos, length=4) != "OggS"
      then
        test.fail(
          "#{mount}: no Ogg page at offset #{pos}"
        )
      end
      segments = byte(data, pos + 26)
      if
        pos + 27 + segments > len
      then
        list.rev(acc)
      else
        size =
          list.fold(
            fun (size, i) -> size + byte(data, pos + 27 + i),
            0,
            list.init(segments, fun (i) -> i)
          )
        next = pos + 27 + segments + size
        if
          next > len
        then
          list.rev(acc)
        else
          begins_link = byte(data, pos + 5) / 2 mod 2 == 1
          parse(next, (begins_link, serial(data, pos))::acc)
        end
      end
    end
  end

  parse(0, [])
end

def check(mount, data) =
  let pages = pages(mount, data)
  if
    list.length(pages) < 5
  then
    test.fail(
      "#{mount}: too few pages"
    )
  end
  let (first_begins_link, first_serial) = list.hd(pages)
  if
    not first_begins_link
  then
    test.fail(
      "#{mount}: first page does not begin a link"
    )
  end
  current = ref(first_serial)
  links = ref(0)
  list.iter(
    fun (p) ->
      begin
        let (begins_link, serial) = p
        if
          begins_link
        then
          ref.incr(links)
          current := serial
        elsif
          serial != current()
        then
          test.fail(
            "#{mount}: page of link #{serial} after the header of link #{
              current()
            }"
          )
        end
      end,
    pages
  )
  if
    links() < 2
  then
    test.fail(
      "#{mount}: no link change seen"
    )
  end
end

def listen(mount) =
  data = ref("")
  try
    http.get.stream(
      timeout=15.,
      on_body_data=fun (chunk) ->
        begin
          data := data() ^ (chunk ?? "")
          if
            string.length(encoding="ascii", data()) > 40000
          then
            error.raise(
              error.not_found,
              "enough data"
            )
          end
        end,
      "http://127.0.0.1:#{port}/#{mount}"
    )
  catch _ do
    ()
  end
  check(mount, data())
end

thread.run(
  delay=12.,
  fun () ->
    begin
      listen("ffmpeg")
      test.pass()
    end
)
