# Regression test for https://github.com/savonet/liquidsoap/issues/3944
# A filter graph must survive its input running dry. `buffer` is unavailable
# while it fills, and a filter with a lookahead, `loudnorm` here, cannot fill a
# frame from what it holds during that window. The graph ends there -- avfilter
# has no way back from end of file -- and has to be built again when the input
# comes back, which is what this checks.

%ifdef ffmpeg.filter.loudnorm
def normalized(s) =
  def mkfilter(graph) =
    a = ffmpeg.filter.audio.input(graph, source.tracks(s).audio)
    a = ffmpeg.filter.loudnorm(i=-14., graph, a)
    a = ffmpeg.filter.audio.output(graph, a)
    source({audio = a})
  end
  ffmpeg.filter.create(mkfilter)
end

s = buffer(sine())
s = ffmpeg.raw.encode.audio(%ffmpeg(%audio.raw), s)
s = normalized(s)
s = ffmpeg.raw.decode.audio(s)

frames = ref(0)
s.on_frame(synchronous=true, fun () -> frames := frames() + 1)

output.dummy(fallible=true, s)

# The buffer runs dry as it fills, well within the first second. Whatever comes
# out after that is a generation of the graph that was built after it.
thread.run(
  delay=5.,
  fun () ->
    begin
      if
        frames() < 25
      then
        test.fail(
          "graph stopped producing after its input ran dry: #{frames()} frames"
        )
      else
        test.pass()
      end
    end
)
%else
test.skip()
%endif
