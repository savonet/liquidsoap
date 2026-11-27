%include "../test.liq"

# A format its destination cannot take is refused when the output is built.

def f() =
  dir = file.temp_dir("encoder_refusals")
  on_cleanup({file.rmdir(dir)})
  s = sine()

  def refused(name, build) =
    try
      build()
      print(
        "#{name}: accepted"
      )
      test.fail()
    catch err : [error.invalid] do
      print(
        "#{name}: #{err.message}"
      )
    end
  end

%ifencoder %flac
  refused(
    "flac append",
    {output.file(start=false, append=true, %flac, "#{dir}/a.flac", s)}
  )
  output.file(start=false, %flac, "#{dir}/b.flac", s)
%endif

  refused(
    "avi append",
    {
      output.file(
        start=false,
        append=true,
        %avi,
        "#{dir}/a.avi",
        (blank() : source)
      )
    }
  )

%ifencoder %mp3
  refused(
    "id3v2 live",
    {output.harbor(start=false, port=9471, mount="a", %mp3(id3v2 = true), s)}
  )
  refused(
    "id3v2 append",
    {
      output.file(
        start=false,
        append=true,
        %mp3(id3v2 = true),
        "#{dir}/a.mp3",
        s
      )
    }
  )
  refused(
    "id3v2 segmented",
    {output.file.hls(start=false, "#{dir}/hls", [("a", %mp3(id3v2 = true))], s)}
  )
  output.file(start=false, %mp3(id3v2 = true), "#{dir}/b.mp3", s)
  output.file(start=false, append=true, %mp3, "#{dir}/c.mp3", s)
  output.harbor(start=false, port=9471, mount="b", %mp3(id3v2 = false), s)
%endif

%ifencoder %fdkaac
  refused(
    "adif live",
    {
      output.harbor(
        start=false,
        port=9471,
        mount="c",
        %fdkaac(transmux = "adif"),
        s
      )
    }
  )
%endif

  test.pass()
end

test.check(f)
