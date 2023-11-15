# HTTP input

Liquidsoap can create a source that pulls its data from an HTTP location. This location can
be a distant file or playlist, or an icecast or shoutcast stream.

To use it in your script, simply create a source that way:

```{.liquidsoap include="http-input.liq" from=1 to=-1}

```

This operator will pull regularly the given location for its data, so it should be used for
locations that are assumed to be available most of the time. If not, it might generate unnecessary
traffic and pollute the logs. In this case, it is perhaps better to inverse the paradigm and
use the [input.harbor](harbor.html) operator.
