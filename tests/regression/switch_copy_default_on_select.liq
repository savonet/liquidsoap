%include "../test.liq"

# The default on_select must hand encoded content through untouched: on the
# first selection, and on a mid-track switch short enough to be sequenced.
settings.source.composition.max_fade := 1000.

file =
  "../media/@ffmpeg[format='mp4',@audio[codec='aac',channels=1],@video[codec='libx264']].mp4"

def f() =
  first_selected = ref(true)
  a = single(file)
  b = single(file)

  b.on_track(synchronous=true, fun (_) -> test.pass())

  s = switch([({first_selected()}, a.{track_sensitive = {false}}), ({true}, b)])

  s.on_track(synchronous=true, fun (_) -> first_selected := false)

  output.file(
    fallible=true,
    %ffmpeg(format = "mp4", %audio.copy, %video.copy),
    "switch_copy_default_on_select.mp4",
    s
  )
end

test.check(f)
