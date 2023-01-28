#!../../liquidsoap ../test.liq

def section(name) =
  print("\n*** #{name} ***\n\n")
end

def incorrect(expr)
  print("Incorrect expression #{expr}...\n")

  try
    let eval _ = expr
    test.fail()
  catch err do
    print("Got err: #{err}")
  end

  print("\n")
end

def correct(expr) =
  print("Correct expression #{expr}...\n")
  let eval _ = expr
  print("\n")
end

def f() =
  section("LISTS")
  incorrect('ignore([4,"x"])')
  correct('ignore([input.harbor("foo"), sine()])')
  correct('ignore([sine(), input.harbor("foo")])')
  correct('ignore([1, ...[2,3,4], ...[5,6], 7])')
  correct('let [x,y,...z] = [1,2]')
  correct('let [] = [1,2]')
  incorrect('let [...z, x, ...t] = [1,2]')

  section("BASIC")
  incorrect('[1]==["1"]')
  incorrect('1==["1"]')
  incorrect('1==(1,"1")')
  # In some of those examples the type error could be reported for a
  # sub-expression since we have location information.
  # With the concise error, it's still pretty good currently.
  incorrect('(1,1)==(1,"1")')
  incorrect('(1,1)==("1",1)')
  incorrect('1==request.create("")')
  incorrect('fun(x)->x(snd(x))')
  correct('{a = 5, b = 3} == {a = 6}')

  correct('true ? "foo" : "bar"')
  incorrect('false ? true : "bar"')

  section("SUBTYPING")
  incorrect('(1:unit)')
  # Next one requires the inference of a subtype (fixed vs. variable arity)
  correct('ignore(stereo(add([])))')
  correct('ignore((blank():source(audio=pcm,video=canvas)))')

  section("CONSTRAINTS")
  incorrect('"bl"+"a"')
  incorrect('(fun(a,b)->a+b)==(fun(a,b)->a+b)')
  incorrect('fun(x)->x(x)') # TODO is it an accident that we get same varname
  incorrect('def f(x) y=snd(x) y(x) end')

  section("LET GENERALIZATION")
  correct('def f(x) = y=x ; y end ignore(f(3)+snd(f((1,2))))')
  incorrect('def f(x) = y=x ; y end ignore(f(3)+"3")')

  section("ARGUMENTS")
  # The errors should be about the type of the param, not of the function.
  incorrect('1+"1"')
  # Also, a special simple error is expected for obvious labelling mistakes.
  incorrect('fallback(transitions=[],xxxxxxxxxxx=[])')
  incorrect('fallback(transitions=[],transitions=[])')

  section("FUNCTIONS")
  # Partial application is not supported anymore
  incorrect('def f(x,y) = y end ignore(f(2))')
  incorrect('fallback(transitions=[fun(~l)->1],[blank()])')
  incorrect('fallback(transitions=[fun(~l=1)->1],[blank()])')
  incorrect('fallback(transitions=[fun(x,y=blank())->y],[blank()])')
  incorrect('fallback(transitions=[fun(x,y)->0],[blank()])')
  correct('fallback(transitions=[fun(x,y,a=2)->x],[blank()])')
  incorrect('fallback(transitions=[fun(x,y)->y+1],[blank()])')
  correct('x=fun(f)->f(3) y=x(fun(f,u="1")->u) ignore(y)')
  incorrect('f=fun(~l,~l) -> l')

  section("CONTENT KIND")
  incorrect('output.file(%vorbis(stereo),"foo",mean(blank()))')
  incorrect('output.file(%vorbis(stereo),"foo",video.add_image(blank()))')
  incorrect('def f(x) = output.file(%vorbis(stereo),"",x) output.file(%vorbis(mono),"",x) end')
  incorrect('add([output.file(%vorbis(stereo),"",blank()),output.file(%vorbis(mono),"",blank())])')
  incorrect('add([mean(blank()),audio_to_stereo(add([]))])')

  section("TRACK TYPES")
  correct('(source.tracks(blank()).audio:pcm)')
  correct('(source.tracks(blank()).audio:canvas)')
  correct('(source.tracks(single("")).midi:midi)')
  correct('(source.tracks(single("")).audio:ffmpeg.copy)')

  section("PATTERNS")
  incorrect("let [x = {foo}, y = (foo), z] = l")
  incorrect("let _.{foo=123} = {foo=123}")
  incorrect("let v.{foo=123} = {foo=123}")

  section("ENCODERS")
  correct('%ffmpeg(%video(codec="h264_nvenc"))')
  correct('%ffmpeg(%video(codec="h264_nvenc",hwaccel="none"))')
  correct('%ffmpeg(%video(codec="h264_nvenc",hwaccel="auto",hwaccel_device="none"))')
  correct('%ffmpeg(%video(codec="h264_nvenc",hwaccel_device="foo"))')
  correct('%ffmpeg(format="mpegts",
                %audio(
                  codec="aac",
                  channels=2,
                  ar=44100
                ))')
  correct('%ffmpeg(format="mpegts",
               %audio(
                 codec="aac",
                 channels=2,
                 ar=44100,
                 b="96k"
               ))')
  correct('%ffmpeg(format="mpegts",
               %audio(
                codec="aac",
                channels=2,
                ar=44100,
                b="192k"
              ))')
  correct('%ffmpeg(
         format="mpegts",
         %audio(
            codec="aac",
            b="128k",
            channels=2,
            ar=44100
         ),
         %video(
           codec="libx264",
           b="5M"
         )
       )')
  correct('%ffmpeg(
        format="mp4",
        movflags="+dash+skip_sidx+skip_trailer+frag_custom",
        frag_duration=10,
        %audio(
          codec="aac",
          b="128k",
          channels=2,
          ar=44100),
        %video(
          codec="libx264",
          b="5M"
        )
      )')
  correct('%ffmpeg(
         format="mpegts",
         %audio.raw(
            codec="aac",
            b="128k",
            channels=2,
            ar=44100
         ),
         %video.raw(
           codec="libx264",
           b="5M"
         )
       )')
  correct('%ffmpeg(
        format="mp4",
        movflags="+dash+skip_sidx+skip_trailer+frag_custom",
        frag_duration=10,
        %audio.raw(
          codec="aac",
          b="128k",
          channels=2,
          ar=44100),
        %video.raw(
          codec="libx264",
          b="5M"
        )
      )')
  correct('%ffmpeg(
         format="mpegts",
         %audio.copy,
         %video.copy)')
  correct('%ffmpeg(
        format="mp4",
        movflags="+dash+skip_sidx+skip_trailer+frag_custom",
        frag_duration=10,
        %audio.copy,
        %video.copy)')
  correct('%ffmpeg(%audio.copy(ignore_keyframe), %video.copy(ignore_keyframe))')
  correct('%ffmpeg(%audio.copy(wait_for_keyframe), %video.copy(wait_for_keyframe))')

  # Conventions for content type in %ffmpeg encoder:
  correct('%ffmpeg(%foo_audio, %bla_video_gni)')
  correct('%ffmpeg(%foo(audio_content), %bla(video_content))')
  incorrect('%ffmpeg(%foo, %bla_gni)')

  correct('%ffmpeg(%foo_audio.raw, %bla_video_gni.raw)')
  correct('%ffmpeg(%foo.raw(audio_content), %bla.raw(video_content))')
  incorrect('%ffmpeg(%foo.raw, %bla_gni.raw)')

  correct('%ffmpeg(%foo_audio.copy, %bla_video_gni.copy)')
  incorrect('%ffmpeg(%foo.copy(audio_content), %bla.copy(video_content))')
  correct('%ffmpeg(%foo.copy, %bla_gni.copy)')

  # The following is not technically checking on type errors but runtime invalid values.
  section("INVALID VALUES")
  incorrect('crossfade(input.http(self_sync=true,"http://foo.bar"))')
  incorrect('crossfade(fallback([input.http("http://foo.bar"), input.http(self_sync=true,"http://foo.bar")]))')
  incorrect('crossfade(sequence([input.http("http://foo.bar"), input.http(self_sync=true,"http://foo.bar")]))')
  incorrect('crossfade(add([input.http("http://foo.bar"), input.http(self_sync=true,"http://foo.bar")]))')

  section("OPTIONAL METHODS")
  incorrect("(({}:{foo?:int}):{foo:int?})")
  incorrect("fun (x) -> x?.foo.gni.gna + 1")
  correct("(({}:{foo?:int}):{foo?:int?})")
  correct("fun (x) -> x?.foo.gni.gna ?? 1")
  correct("fun (x) -> x.foo.gni?.bla(1,2,3).blo")
  correct("fun (x) -> x?.foo(123).gni ?? null()")

  section("EVAL")
  incorrect("let eval x = '123'
ignore(x+1)
ignore(x / 2.)")

  section("PATTERNS")
  incorrect("
  def f(x) =
    let y.{foo} = x
    y.foo + 1
    end
  ")

  incorrect('
  def g() =
    s = single("foo")

    s = s

    enc = %ffmpeg(
      format="mkv",
      %audio(codec="aac"),
      %video(codec="aac")
    )

    output.file(fallible=true, enc, "/tmp/bla.mkv", s)

    ignore(ffmpeg.decode.audio(s))
  end')

  section("FORMATS")

  audio_formats_with_stereo = ["external(process=\"gni\",", "flac(", "opus(", "shine(", "vorbis(", "mp3("]
  def check_stereo_syntax(format) =
    correct("%#{format}mono)")
    correct("%#{format}stereo)")
    correct("%#{format}mono=false)")
    correct("%#{format}stereo=false)")
  end
  list.iter(check_stereo_syntax, audio_formats_with_stereo)

  test.pass()
end

test.check(f)
