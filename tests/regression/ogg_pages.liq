%include "../test.liq"

# `bytes_per_page` sets the size of data pages. It had no effect: the binding
# only honoured it behind a compile flag nothing defined. Ogg files with audio
# and video started with a link holding only a skeleton, which ffprobe cannot
# read; no skeleton is written.

pending = ref(3)

def done() =
  pending := pending() - 1
  if pending() == 0 then test.pass() end
end

def pages(path) =
  list.length(string.split(separator="OggS", file.contents(path))) - 1
end

def run(name, format, check) =
  s = once(sine(duration=2.))
  s = source.mux.video(video=video.fill(blank()), s)
  clock.assign_new(sync="none", [s])

  def on_close(path) =
    if check(path) then done() else test.fail() end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    format,
    file.temp(cleanup=true, "ogg_pages_#{name}", ".ogg"),
    drop_video(s)
  )
end

default_pages = ref(null)
small_pages = ref(null)

def compare() =
  if
    null.defined(default_pages()) and null.defined(small_pages())
  then
    d = null.get(default_pages())
    n = null.get(small_pages())
    print(
      "pages: #{d} by default, #{n} with bytes_per_page=500"
    )
    n >= 2 * d
  else
    true
  end
end

def record(r, path) =
  r := pages(path)
  compare()
end

run("default", %ogg(%vorbis), fun (path) -> record(default_pages, path))
run(
  "small",
  %ogg(%vorbis(bytes_per_page = 500)),
  fun (path) -> record(small_pages, path)
)

def audio_video(name, format) =
  s = once(sine(duration=2.))
  s = source.mux.video(video=video.fill(blank()), s)
  clock.assign_new(sync="none", [s])

  def on_close(path) =
    streams =
      process.read.lines(
        "ffprobe -v error -show_entries stream=codec_name -of csv=p=0 #{
          process.quote(path)
        }"
      )

    streams = list.filter(fun (s) -> s != "", streams)
    print(
      "#{name} streams: #{streams}"
    )
    if
      streams == ["theora", "vorbis"] or streams == ["vorbis", "theora"]
    then
      done()
    else
      test.fail()
    end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    format,
    file.temp(cleanup=true, "ogg_pages_#{name}", ".ogg"),
    s
  )
end

%ifencoder %theora
audio_video("av", %ogg(%vorbis, %theora))
%else
done()
%endif
