# Smart crossfade

## Basic operator

Liquidsoap includes an advanced crossfading operator. Using it, you can code which transition you want for your songs, according to the average volume level (in dB) computed on the end of the ending track and the beginning of the new one.

The low level operator is `cross`. With it, you can register a function that returns the transition you like. The arguments passed to this function are:

- volume level for previous track
- volume level for next track
- metadata chunk for previous track
- metadata chunk for next track
- source corresponding to previous track
- source corresponding to next track

You can find its documentation in the [language reference](reference.html).

## Example

Liquidsoap also includes a ready-to-use operator defined using `cross`, it is called `crossfade` and is defined in the pervasive helper script `utils.liq`. Its code is:

```{.liquidsoap include="content/liq/crossfade.liq"}

```

You can use it directly in your script, or use this code to define yours!
