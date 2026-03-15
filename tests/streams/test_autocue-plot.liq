base_dir = path.dirname(argv(0))

tests =
  [
    (
      {
        cue_in=0.,
        cue_out=7.,
        fade_in=2.,
        fade_out_type="lin",
        fade_out=1.,
        start_next=2.
      },
      {cue_in=0., cue_out=10., fade_in=3., fade_in_type="lin", fade_out=2.}
    ),
    (
      {
        cue_in=0.,
        cue_out=7.,
        fade_in=2.,
        fade_out_type="lin",
        fade_out=5.,
        start_next=6.
      },
      {cue_in=0., cue_out=10., fade_in=3., fade_in_type="lin", fade_out=2.}
    ),
    (
      {
        cue_in=0.,
        cue_out=7.,
        fade_in=2.,
        fade_out_type="lin",
        fade_out=3.,
        start_next=4.
      },
      {cue_in=0., cue_out=10., fade_in=1., fade_in_type="lin", fade_out=2.}
    )
  ]

def rec execute(pos) =
  dir = file.temp_dir("plot")
  on_cleanup({file.rmdir(dir)})

  def on_stop() =
    file.copy(
      path.concat(dir, "old.txt"),
      path.concat(base_dir, "autocue-plot.#{pos}.old.txt.gen")
    )
    file.copy(
      path.concat(dir, "new.txt"),
      path.concat(base_dir, "autocue-plot.#{pos}.new.txt.gen")
    )
    if pos == list.length(tests) - 1 then test.pass() else execute(pos + 1) end
  end

  let (old_autocue, new_autocue) = list.nth(tests, pos)

  autocue.plot(
    dir=dir,
    png="autocue-plot.#{pos}.png.gen",
    old_autocue=old_autocue,
    new_autocue=new_autocue,
    sync="none",
    on_stop=on_stop
  )
end

execute(0)
