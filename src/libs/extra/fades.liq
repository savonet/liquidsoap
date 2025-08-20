# Plot the first crossfade transition. Used for visualizing and testing
# crossfade transitions.
# @category Source / Track processing
# @flag extra
def cross.plot(~png=null, ~dir=null, s) =
  dir =
    if
      null.defined(dir)
    then
      null.get(dir)
    else
      dir = file.temp_dir("plot")
      on_cleanup({file.rmdir(dir)})
      dir
    end

  old_txt = path.concat(dir, "old.txt")
  new_txt = path.concat(dir, "new.txt")

  def gnuplot_cmd(filename) =
    'set term png; set output "#{(filename : string)}"; plot "#{new_txt}" using \
     1:2 with lines title "new track", "#{old_txt}" using 1:2 with lines title \
     "old track"'
  end

  def store_rms(~id, s) =
    s = rms(duration=settings.frame.duration(), s)
    t0 = ref(null)

    s.on_frame(
      synchronous=true,
      before=false,
      {
        let t0 =
          if
            null.defined(t0())
          then
            null.get(t0())
          else
            t0 := source.time(s)
            null.get(t0())
          end
        let v = s.rms()
        let p = source.time(s) - t0
        fname = id == "old" ? old_txt : new_txt
        file.write(append=true, data="#{p}\t#{v}\n", fname)
      }
    )

    s
  end

  plotted = ref(false)

  def transition(old, new) =
    old = store_rms(id="old", fade.out(old.source))
    new = store_rms(id="new", fade.in(new.source))

    s = blank(duration=0.1)
    s.on_frame(
      synchronous=true,
      {
        if
          null.defined(png) and not plotted()
        then
          ignore(
            process.run(
              "gnuplot -e #{process.quote(gnuplot_cmd(null.get(png)))}"
            )
          )
        end
        plotted := true
      }
    )

    sequence(single_track=false, [add(normalize=false, [new, old]), once(s)])
  end

  cross(transition, s)
end

# Plot two sine tracks with given autocue params.
# Used for visualizing and testing autocue crossfade transitions.
# @category Source / Track processing
# @flag extra
def autocue.plot(
  ~png=null,
  ~dir=null,
  ~old_autocue,
  ~new_autocue,
  ~sync="auto",
  ~on_stop
) =
  dir =
    if
      null.defined(dir)
    then
      null.get(dir)
    else
      dir = file.temp_dir("plot")
      on_cleanup({file.rmdir(dir)})
      dir
    end

  old_txt = path.concat(dir, "old.txt")
  new_txt = path.concat(dir, "new.txt")

  def gnuplot_cmd(filename) =
    'set term png; set output "#{(filename : string)}"; plot "#{new_txt}" using \
     1:2 with lines title "new track", "#{old_txt}" using 1:2 with lines title \
     "old track"'
  end

  def autocue_annotate(a, uri) =
    meta = autocue.metadata(implementation="autocue.plot", a)
    meta =
      list.map(fun ((label, value)) -> "#{label}=#{string.quote(value)}", meta)
    meta = string.concat(separator=",", meta)
    "annotate:#{meta}:#{uri}"
  end

  old_uri =
    process.uri(
      extname="wav",
      "ffmpeg -f lavfi -i \"sine=frequency=1000:duration=10\" -y $(output) < \
       /dev/null"
    )
  old_uri = autocue_annotate(old_autocue, old_uri)
  old_source = request.once(request.create(old_uri))

  new_uri =
    process.uri(
      extname="wav",
      "ffmpeg -f lavfi -i \"sine=frequency=500:duration=10\" -y $(output) < \
       /dev/null"
    )
  new_uri = autocue_annotate(new_autocue, new_uri)
  new_source = request.once(request.create(new_uri))

  s = sequence([old_source, new_source])

  def store_rms(~fname, ~offset, s) =
    s = rms(duration=settings.frame.duration(), s)

    s.on_frame(
      synchronous=true,
      before=false,
      {
        let t = source.time(s) - offset
        let v = s.rms()
        file.write(append=true, data="#{t}\t#{v}\n", fname)
      }
    )

    s
  end

  is_first_ending = ref(true)
  is_first_starting = ref(true)

  def transition(old, new) =
    list.iter(
      fun (x) ->
        log(
          level=4,
          "Before: #{x}"
        ),
      metadata.cover.remove(old.metadata)
    )

    list.iter(
      fun (x) ->
        log(
          level=4,
          "After : #{x}"
        ),
      metadata.cover.remove(new.metadata)
    )

    offset = source.duration(old.source) + source.duration(new.source)

    def ending_map(s) =
      if
        is_first_ending()
      then
        is_first_ending := false
        store_rms(fname=old_txt, offset=offset, s)
      else
        s
      end
    end

    def starting_map(s) =
      if
        is_first_starting()
      then
        is_first_starting := false
        store_rms(fname=new_txt, offset=offset, s)
      else
        s
      end
    end

    cross.simple(
      initial_fade_in_metadata=new.metadata,
      initial_fade_out_metadata=old.metadata,
      ending_map=ending_map,
      starting_map=starting_map,
      old.source,
      new.source
    )
  end

  s = cross(transition, s)

  clock.assign_new(sync=sync, [s])

  def on_stop() =
    if
      null.defined(png)
    then
      ignore(
        process.run(
          "gnuplot -e #{process.quote(gnuplot_cmd(null.get(png)))}"
        )
      )
    end
    on_stop()
    clock(s.clock).stop()
  end

  o = output.dummy(fallible=true, s)
  o.on_stop(synchronous=true, on_stop)
end
