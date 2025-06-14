# Plot the first crossfade transition. Used for visualizing and testing
# crossfade transitions.
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

    source.on_frame(
      before=false,
      s,
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
  end

  plotted = ref(false)

  def transition(old, new) =
    old = store_rms(id="old", fade.out(old.source))
    new = store_rms(id="new", fade.in(new.source))

    s = blank(duration=0.1)
    s =
      source.on_frame(
        s,
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
