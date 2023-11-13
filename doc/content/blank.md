# Blank detection

[Liquidsoap](index.html) has three operators for dealing with blanks.

On GeekRadio, we play many files, some of which include bonus tracks, which
means that they end with a very long blank and then a little extra music. It's
annoying to get that on air. The `blank.skip` operator skips the
current track when a too long blank is detected, which avoids that. The typical
usage is simple:

```liquidsoap
# Wrap it with a blank skipper
s = blank.skip(s)
```

At [RadioPi](http://www.radiopi.org/) they have another problem: sometimes they
have technical problems, and while they think they are doing a live show,
they're making noise only in the studio, while only blank is on air; sometimes,
the staff has so much fun (or is it something else ?) doing live shows that they
leave at the end of the show without thinking to turn off the live, and the
listeners get some silence again. To avoid that problem we made the
`blank.strip` operators which hides the stream when it's too blank
(i.e. declare it as unavailable), which perfectly suits the typical setup used
for live shows:

```{.liquidsoap include="content/liq/blank-sorry.liq"}

```

If you don't get the difference between these two operators, you should learn
more about liquidsoap's notion of [source](sources.html).

Finally, if you need to do some custom action when there's too much blank, we
have `blank.detect`:

```{.liquidsoap include="content/liq/blank-detect.liq" from=1 to=-1}

```
