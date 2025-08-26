# Basic request tests
settings.log.level.set(4)
test.check(
  fun () ->
    begin
      test.almost_equal(digits=1, request.duration("file1.mp3") ?? 0., 5.06)
      test.almost_equal(
        digits=1,
        request.duration("annotate:cue_out=1.1:file1.mp3") ?? 0.,
        1.1
      )
      test.almost_equal(
        digits=1,
        request.duration("annotate:cue_in=1.06:file1.mp3") ?? 0.,
        4.
      )
      test.almost_equal(
        digits=1,
        request.duration("annotate:cue_in=1.00,cue_out=3.1:file1.mp3") ?? 0.,
        2.1
      )

      r = request.dynamic(prefetch=1, fun () -> request.create("invalid"))
      if
        not r.add(request.create("file2.mp3"))
      then
        print(
          "Failed to add request"
        )
        test.fail()
      end

      r.set_queue([request.create("file2.mp3"), request.create("file3.mp3")])
      q = r.queue()
      if
        list.length(q) != 2
      then
        print(
          "Invalid request queue"
        )
        test.fail()
      end

      def next() =
        request.create("file1.mp3")
      end

      r = request.dynamic(prefetch=1, next)
      o = output.dummy(fallible=true, r)
      thread.run.recurrent(
        fun () ->
          if
            not r.is_ready()
          then
            0.1
          else
            c = r.current()
            if
              not null.defined(c)
            then
              print(
                "Invalid current track"
              )
              test.fail()
            end

            c = null.get(c)
            if
              request.filename(c) != "file1.mp3"
            then
              print(
                "Invalid filename"
              )
              test.fail()
            end

            o.stop()
            test.pass()
            (-1.)
          end
      )
    end
)
