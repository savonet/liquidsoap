# Crossfade

## Out of the box

Liquidsoap provides a default `crossfade` operator out of the box. It is a simple operator that does the work and does it well!

Over the years, we have realized that crossfading is a very sensitive topic and that people care a lot about specific details and how well
it is done.

Since release `2.2.5`, liquidsoap integrates an automated mechanism to compute crossfade transitions that was contributed by our users.

If you have the `ffmpeg` bindings enabled, all you should need to do to enable this feature is adding the following to your script:

```liquidsoap
enable_autocue_metadata()
```

This uses the default, internal implementation. If you want more control over the automated crossfade parameters, you can
check out the external [autocue](https://github.com/Moonbase59/autocue) implementation and its associated documentation.

## Custom crossfades

You can also define your own crossfade transitions if you want to be more specific about them! The base `cross` operator accepts a scripted transition function that,
according to the average volume level (in dB) computed on the end of the ending track and the beginning of the new one, returns the transition that is desired.

You can find its documentation in the [language reference](reference.html).

Here's an example:

```{.liquidsoap include="crossfade.liq"}

```
