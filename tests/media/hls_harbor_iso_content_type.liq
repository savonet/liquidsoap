%include "../test.liq"
%include "hls_lib.liq"

# serving.md §4.1: every ISO family muxer's media segments are served as
# video/iso.segment.
port = 18768
dir = file.temp_dir(cleanup=true, "hls-harbor-iso-content-type")
s = sine()
clock.assign_new(sync="none", [s])

o =
  output.harbor.hls(
    port=port,
    path="live",
    tmpdir=dir,
    segment_duration=1.,
    [
      (
        "mov",
        %ffmpeg(
          format = "mov",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      ),
      (
        "ismv",
        %ffmpeg(
          format = "ismv",
          %audio(codec = "aac", samplerate = 44100, channels = 2)
        )
      )
    ],
    s
  )

def check(name) =
  r = http.get("http://127.0.0.1:#{port}/live/#{name}_2.m4s")
  hls_test.check(
    r.status_code == 200,
    "#{name}: status #{r.status_code}"
  )
  content_type =
    list.find(
      default=("", ""),
      fun (h) -> string.case(lower=true, fst(h)) == "content-type",
      r.headers
    )
  content_type = snd(content_type)
  hls_test.check(
    content_type == "video/iso.segment",
    "#{name}: Content-Type #{string.quote(content_type)}"
  )
end

started = ref(false)
o.on_file_change(
  synchronous=true,
  fun (ev) ->
    if
      not started()
      and path.basename(ev.path) == "ismv_3.m4s"
      and file.exists("#{dir}/mov_2.m4s")
    then
      started := true
      thread.run(
        fun () ->
          begin
            check("mov")
            check("ismv")
            hls_test.done(4)
          end
      )
    end
)
