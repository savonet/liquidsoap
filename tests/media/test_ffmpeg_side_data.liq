%include "../test.liq"

log.level.set(4)
settings.decoder.decoders.set(["ffmpeg"])

# 320x240, stored with a display rotation of 90.
input = "rotated.mp4"

def probe(fname) =
  let json.parse (parsed :
    {streams: [{width: int, height: int, side_data_list: [{rotation: int?}]?}]}
  ) = test.ffprobe(fname)

  let [stream] = parsed.streams
  rotation =
    list.fold(
      fun (rotation, entry) -> entry.rotation ?? rotation,
      0,
      stream.side_data_list ?? []
    )

  {width = stream.width, height = stream.height, rotation = rotation}
end

def edit_video(s, f) =
  tracks = source.tracks(s)
  source(tracks.{video = f(tracks.video)})
end

read_rotation = ref(fun () -> null)
undecided = ref([])
rotations = ref([])
todo = ref(7)

# Whether an angle is the expected one, in either direction.
def near(angle, expected) =
  angle = abs(angle)
  abs(min(angle, 360. - angle) - expected) < 2.
end

# odd.mp4 and skewed.mp4 are stored with rotations of 33 and 20 degrees, which
# are not quarter turns. The first is decided here, the second is left alone.
ffmpeg.autorotate.on_undecided(
  fun (question) ->
    begin
      undecided := [...undecided(), question.reason]
      rotation = question.rotation ?? 0.
      rotations := [...rotations(), rotation]
      if
        near(rotation, 20.)
      then
        null
      else
        [("transpose", [("dir", "clock")])]
      end
    end
)

def check() =
  copied = probe("side-data-copy.mp4")
  turned = probe("side-data-turned.mp4")
  removed = probe("side-data-removed.mp4")
  decoded = probe("side-data-decoded.mp4")
  encoded = probe("side-data-encoded.mp4")
  read = read_rotation()

  test.equal(copied, {width = 320, height = 240, rotation = 90})
  test.equal(read() ?? 0., 90.)
  test.equal(abs(turned.rotation), 180)
  test.equal(removed.rotation, 0)
  test.equal(decoded, {width = 240, height = 320, rotation = 0})
  test.equal(encoded.rotation, 90)

  # Each decoder of the file asks once.
  test.equal(list.length(undecided()) > 0, true)
  test.equal(
    list.for_all(fun (reason) -> reason == "odd_rotation", undecided()),
    true
  )
  test.equal(
    list.for_all(
      fun (rotation) -> near(rotation, 33.) or near(rotation, 20.),
      rotations()
    ),
    true
  )
  test.equal(probe("side-data-odd.mp4").rotation, 0)

  # No decision: the video keeps its rotation.
  test.equal(near(float(probe("side-data-skewed.mp4").rotation), 20.), true)
  test.pass()
end

def on_close(_) =
  left =
    atomic(
      fast=true,
      fun () ->
        begin
          todo := todo() - 1
          todo()
        end
    )

  if left == 0 then check() end
end

def run(name, encoder, s) =
  output.file(
    fallible=true,
    on_close=on_close,
    encoder,
    "side-data-#{name}.mp4",
    s
  )
end

copy = %ffmpeg(format = "mp4", %video.copy)

run(
  "copy",
  copy,
  edit_video(
    once(single(input)),
    fun (video) ->
      begin
        video = track.ffmpeg.side_data(video)
        read_rotation := video.rotation
        video
      end
  )
)

run(
  "turned",
  copy,
  edit_video(
    once(single(input)),
    fun (video) -> track.ffmpeg.side_data(rotation=180., video)
  )
)

run(
  "removed",
  copy,
  edit_video(
    once(single(input)),
    fun (video) ->
      track.ffmpeg.side_data(
        remove=[
          "Display Matrix"
        ],
        video
      )
  )
)

run(
  "decoded",
  %ffmpeg(format = "mp4", %video(codec = "mpeg4")),
  once(single(input))
)

# Decoding removed the rotation: the encoder gets the one set here.
run(
  "encoded",
  %ffmpeg(format = "mp4", %video.raw(codec = "mpeg4")),
  edit_video(
    once(single(input)),
    fun (video) -> track.ffmpeg.raw.side_data(rotation=90., video)
  )
)

run(
  "odd",
  %ffmpeg(format = "mp4", %video.raw(codec = "mpeg4")),
  once(single("odd.mp4"))
)

run(
  "skewed",
  %ffmpeg(format = "mp4", %video.raw(codec = "mpeg4")),
  once(single("skewed.mp4"))
)
