LADSPA plugins in Liquidsoap
============================
[LADSPA](http://www.ladspa.org/) is a standard that allows software audio processors and effects to be plugged into a
wide range of audio synthesis and recording packages.

If enabled, Liquidsoap supports LADSPA plugins. In this case,
installed plugins are detected at run-time and are all available in Liquidsoap under a name
of the form: `ladspa.plugin`, for instance `ladspa.karaoke`, `ladspa.flanger` etc..

The full list of those operators can be found using `liquidsoap --list-plugins`.
Also, as usual, `liquidsoap -h ladspa.plugin` returns a detailed description of each LADSPA's operators.
For instance:

```
./liquidsoap -h ladspa.flanger
*** One entry in scripting values:
Flanger by Steve Harris <steve@plugin.org.uk>.
Category: Source / Sound Processing
Type: (?id:string,?delay_base:'a,?feedback:'b,
 ?lfo_frequency:'c,?max_slowdown:'d,
 source(audio='#e,video='#f,midi='#g))->
source(audio='#e,video='#f,midi='#g)
where 'a, 'b, 'c, 'd is either float or ()->float
Flag: hidden
Parameters:
* id : string (default "")
    Force the value of the source ID.
* delay_base : anything that is either float or ()->float (default 6.32499980927)
    Delay base (ms) (0.1 <= delay_base <= 25).
* feedback : anything that is either float or ()->float (default 0.)
    Feedback (-1 <= feedback <= 1).
* lfo_frequency : anything that is either float or ()->float (default 0.334370166063)
    LFO frequency (Hz) (0.05 <= lfo_frequency <= 100).
* max_slowdown : anything that is either float or ()->float (default 2.5)
    Max slowdown (ms) (0 <= max_slowdown <= 10).
* (unlabeled) : source(audio='#e,video='#f,midi='#g) (default None)
```

For advanced users, it is worth nothing that most of the parameters associated with LADSPA operators
can take a function, for instance in the above: ```
max_slowdown : anything that is either float or ()->float```
.
This means that those parameters may be dynamically changed while running a liquidsoap script.
