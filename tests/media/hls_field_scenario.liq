%include "../test.liq"
%include "hls_lib.liq"

# KC3, KC4: copied H.264 with B-frames, a 2 s GOP and 6 s segments, in three
# video variants with AAC-LC or HE-AAC audio and an audio-only variant. The
# fixture puts its key frames after or before each boundary.
fixture = argv(1)
dir = file.temp_dir(cleanup=true, "hls-field")

s = once(single(fixture))
clock.assign_new(sync="none", [s])

names = ["v1", "v2", "v3", "audio"]

o =
  output.file.hls(
    fallible=true,
    segment_duration=6.,
    segments=100,
    segments_overhead=null,
    dir,
    [
      (
        "v1",
        %ffmpeg(
          format = "mpegts",
          %audio.copy,
          %audio_2.drop,
          %video.copy,
          %video_2.drop,
          %video_3.drop
        )
      ),
      (
        "v2",
        %ffmpeg(
          format = "mpegts",
          %audio.drop,
          %audio_2.copy,
          %video.drop,
          %video_2.copy,
          %video_3.drop
        )
      ),
      (
        "v3",
        %ffmpeg(
          format = "mpegts",
          %audio.copy,
          %audio_2.drop,
          %video.drop,
          %video_2.drop,
          %video_3.copy
        )
      ),
      (
        "audio",
        %ffmpeg(
          format = "mpegts",
          %audio.drop,
          %audio_2.copy,
          %video.drop,
          %video_2.drop,
          %video_3.drop
        )
      )
    ],
    s
  )

video_frame = 1. / 25.
tolerance = video_frame + 0.001

def check() =
  playlists =
    list.map(fun (n) -> (n, hls_test.playlist("#{dir}/#{n}.m3u8")), names)
  let (_, reference) = list.hd(playlists)
  ref_starts = hls_test.starts(reference.segments)
  count = list.length(reference.segments)
  hls_test.check(
    count >= 8,
    "only #{count} segments in v1"
  )

  list.iter(
    fun (np) ->
      begin
        let (name, p) = np
        n = list.length(p.segments)
        hls_test.check(
          n == count,
          "#{name} has #{n} segments, v1 has #{count}"
        )
        hls_test.check(
          p.media_sequence == reference.media_sequence,
          "#{name} media sequence #{p.media_sequence}, v1 #{
            reference.media_sequence
          }"
        )

        # The first segment of the run and the last one, closed at the stop, may
        # be of any length.
        list.iteri(
          fun (i, seg) ->
            if
              i > 0 and i < n - 1
            then
              hls_test.check(
                abs(seg.duration - 6.) <= tolerance,
                "#{name} segment #{i} (#{seg.uri}) EXTINF #{seg.duration}, \
                 expected 6 within one video frame: #{
                  list.map(fun (s) -> s.duration, p.segments)
                }"
              )
            end,
          p.segments
        )

        # conformance.md §2: within the key-frame offset, the same at every
        # segment.
        starts = hls_test.starts(p.segments)
        first_offset = ref(null)
        list.iteri(
          fun (i, t) ->
            if
              i > 0 and i < count
            then
              offset = t - list.nth(ref_starts, i)
              hls_test.check(
                abs(offset) <= 0.5,
                "segment #{i} of #{name} is #{offset}s from v1, beyond the \
                 key-frame tolerance"
              )

              if
                null.defined(first_offset())
              then
                hls_test.check(
                  abs(offset - null.get(first_offset())) <= tolerance,
                  "segment #{i} of #{name} is #{offset}s from v1, segment 1 was #{
                    null.get(first_offset())
                  }s: the variants drift"
                )
              else
                first_offset := offset
              end
            end,
          starts
        )
      end,
    playlists
  )

  hls_test.done(count * 10)
end

hls_test.at_first_stop(o, check)
