## For 2.2

### Done

- Separate language core (#2397)
- Online version (#2397)
  - Available at: https://www.liquidsoap.info/try/
  - Needs some cleanup, definition of a minimal JS library.
- Switch to `dune`
- Separate standard library (in pure liq)

### Ongoing

- support for multi-track audio
- support for subtitles
- Switch to immutable content
- live switch with ffmpeg encoded content

## FOSDEM 2023

- use source getters for switch in order to be able to play two tracks ever day
  (#2880)
- use OCaml 5 threads (#2879)
- add support for caching of types (#2878)
- remove requests and use sources instead everywhere (a request is a source with
  one track [or more])
- use naive (as in native.liq) implementation of switch (based on
  source.dynamic)
- deprecate "!" and ":=" in favor of x.get / x.set

## Backlog

- refine video support in order to have next liquidshop running on Liquidsoap
  (dogfooding)
- native RTMP support (and ensure that HLS output is easy to use)
- update the Liquidsoap book
- switch to immutable content for frames (#2364)
  - frame should be changed to extensible arrays (a bit like `Strings`) instead
    of filling a buffer
  - take the opportunity to change the handling of track boundaries (currently
    boundary = we have a partial fill, which has quite messy corner cases)
- rewrite the clock system
  - the code is unreadable and overengineered ⇒ simplify it
  - we want to get rid of the assumption clock = thread
- rewrite switch / sequence / etc. operators based on only one binary operator:
  fallback
  - note: predicates can ben encoded in availablility
  - transitions might be tricky... we want to make them on the Liquidsoap side
    using cross and a tag system to know from which source we come
- use row variables for methods, using Garrigue's _Simple Type Inference for
  Structural Polymorphism_
- modules (#1934 based on runtime types, not ideal): we can implement them as
  records
  - import
  - support for hiding fields
- ocaml5 support
  - we don't seem to have bigarrays anymore?
- can we reimplement something like [melt](https://www.mltframework.org/)?
