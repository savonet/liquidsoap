# LADSPA plugins in Liquidsoap

[LADSPA](https://www.ladspa.org/) is a standard that allows software audio processors and effects to be plugged into a
wide range of audio synthesis and recording packages.

If enabled, Liquidsoap supports LADSPA plugins. In this case,
installed plugins are detected at run-time and are all available in Liquidsoap under a name
of the form: `ladspa.plugin`, for instance `ladspa.karaoke`, `ladspa.flanger`, etc.

The full list of those operators can be found using `liquidsoap --list-functions | grep '^ladspa.'`.
Also, as usual, `liquidsoap -h ladspa.plugin` returns a detailed description of each LADSPA operator,
including its parameters and their ranges. For instance, `liquidsoap -h ladspa.flanger` starts with:

```
Flanger by Steve Harris <steve(at)plugin.org.uk>.

Type: (?id : string?, ?delay_base : {float}, ?feedback : {float},
 ?lfo_frequency : {float}, ?max_slowdown : {float},
 source(audio=pcm('a), 'b)) -> source(audio=pcm('a), 'b)
```

For advanced users, it is worth noting that the parameters associated with LADSPA operators
are getters, for instance `max_slowdown : {float}` in the above: they accept either a float or
a function returning a float.
This means that those parameters may be dynamically changed while running a liquidsoap script.
