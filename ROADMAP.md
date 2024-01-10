## For 2.2

### Done

- Separate language core (#2397)
- Online version (#2397)
  - Available at: https://www.liquidsoap.info/try/
  - Needs some cleanup, definition of a minimal JS library.
- Switch to `dune`
- Separate standard library (in pure liq)
- support for multi-track audio
- live switch with ffmpeg encoded content
- deprecate "!" and ":=" in favor of x.get / x.set

## For 2.2

- ~~switch to immutable content for metadata~~
- ~~Add script tooling, prettier etc.~~
- ~~switch to immutable content for frames (#2364)~~
  - ~~frame should be changed to extensible arrays (a bit like `Strings`) instead
    of filling a buffer~~
  - ~~take the opportunity to change the handling of track boundaries (currently
    boundary = we have a partial fill, which has quite messy corner cases)~~

## For 2.3

~~- Rewrite streaming loop~~
- rewrite the clock system
  - the code is unreadable and overengineered ⇒ simplify it
  - we want to get rid of the assumption clock = thread
- use OCaml 5 threads (#2879)
- update the Liquidsoap book

## For 2.4

- Optimize runtime: start time, typing and memory usage
- Add support for modules, load minimal API by default
- remove requests and use sources instead everywhere (a request is a source with
  one track [or more])
  - Precise scheduling with queue.push, etc.: we could make the track available
    at some precise time if requests were sources...
  - this may allow stuff like `append` more easily

## FOSDEM 2023 TODO

- use source getters for switch in order to be able to play two tracks ever day
  (#2880)
- use naive (as in native.liq) implementation of switch (based on
  source.dynamic)
- rework buffer.adaptative
- allow showing graphs (of buffer.adaptative for instance)
- reimplement video.tile in native liq


## Backlog

- Simple mechanism to tell source how much data will be expected in advance (e.g. 10s with cross) to allow automatic buffer management.
- Redefine switch-based transitions.
- javascrtipt/browser support using [WebCodecs](https://developer.mozilla.org/en-US/docs/Web/API/WebCodecs_API)!
- support for subtitles
- refine video support in order to have next liquidshop running on Liquidsoap
  (dogfooding)
- native RTMP support (and ensure that HLS output is easy to use)
- rewrite switch / sequence / etc. operators based on only one binary operator:
  fallback
  - note: predicates can ben encoded in availability
  - transitions might be tricky... we want to make them on the Liquidsoap side
    using cross and a tag system to know from which source we come
- use row variables for methods, using Garrigue's _Simple Type Inference for
  Structural Polymorphism_
- can we reimplement something like [melt](https://www.mltframework.org/)?
