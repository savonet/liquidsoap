Smart crossfade
===============
Basic operator
--------------
Liquidsoap includes an advanced crossfading operator. Using it, you can code which transition you want for your songs, according to the average volume level (in dB) computed on the end of the ending track and the beginning of the new one.

The low level operator is `cross`. With it, you can register a function that returns the transition you like. The arguments passed to this function are:

* volume level for previous track
* volume level for next track
* metadata chunk for previous track
* metadata chunk for next track
* source corresponding to previous track
* source corresponding to next track

You can find its documentation in the [language reference](reference.html).

Example
-------
Liquidsoap also includes a ready-to-use operator defined using `cross`, it is called `crossfade` and is defined in the pervasive helper script `utils.liq`. Its code is:

```liquidsoap
# Smart transition for crossfade
# @category Source / Track Processing
# @param ~log Default logger
# @param ~fade_in  Fade-in duration, if any.
# @param ~fade_out Fade-out duration, if any.
# @param ~high     Value, in dB, for loud sound level.
# @param ~medium   Value, in dB, for medium sound level.
# @param ~margin   Margin to detect sources that have too different sound level for crossing.
# @param ~default Smart crossfade: transition used when no rule applies (default: sequence).
# @param a Ending track
# @param b Starting track
def cross.smart(~log=log(label="cross.smart"),
                ~fade_in=3.,~fade_out=3.,
                ~default=(fun (a,b) -> (sequence([a, b]):source)),
                ~high=-15., ~medium=-32., ~margin=4.,
                a, b)
  let fade.out = fade.out(type="sin",duration=fade_out)
  let fade.in  = fade.in(type="sin",duration=fade_in)
  add = fun (a,b) -> add(normalize=false,[b, a])

  # This is for the type system..
  ignore(a.metadata["foo"])
  ignore(b.metadata["foo"])

  if
    # If A and B are not too loud and close, fully cross-fade them.
    a.db_level <= medium and b.db_level <= medium and abs(a.db_level - b.db_level) <= margin
    then
      log("Old <= medium, new <= medium and |old-new| <= margin.")
      log("Old and new source are not too loud and close.")
      log("Transition: crossed, fade-in, fade-out.")
      add(fade.out(a.source),fade.in(b.source))

  elsif
    # If B is significantly louder than A, only fade-out A.
    # We don't want to fade almost silent things, ask for >medium.
    b.db_level >= a.db_level + margin and a.db_level >= medium and b.db_level <= high
  then
    log("new >= old + margin, old >= medium and new <= high.")
    log("New source is significantly louder than old one.")
    log("Transition: crossed, fade-out.")
    add(fade.out(a.source),b.source)

  elsif
    # Opposite as the previous one.
    a.db_level >= b.db_level + margin and b.db_level >= medium and a.db_level <= high
    then
    log("old >= new + margin, new >= medium and old <= high")
    log("Old source is significantly louder than new one.")
    log("Transition: crossed, fade-in.")
    add(a.source,fade.in(b.source))

  elsif
    # Do not fade if it's already very low.
    b.db_level >= a.db_level + margin and a.db_level <= medium and b.db_level <= high
  then
    log("new >= old + margin, old <= medium and new <= high.")
    log("Do not fade if it's already very low.")
    log("Transition: crossed, no fade.")
    add(a.source,b.source)

  # What to do with a loud end and a quiet beginning ?
  # A good idea is to use a jingle to separate the two tracks,
  # but that's another story.

  else
    # Otherwise, A and B are just too loud to overlap nicely, or the
    # difference between them is too large and overlapping would completely
    # mask one of them.
    log("No transition: using default.")
    default(a.source, b.source)
  end
end
```

You can use it directly in your script, or use this code to define yours!
