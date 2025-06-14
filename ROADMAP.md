## Backlog

- Explore new compiled backends
- Update the book
  - Romain to document new internals
- Write article for ICFP
- support for ffmpeg subtitles
- use OCaml 5 (after it has matured)
- use native (as in native.liq) implementation of switch (based on
  source.dynamic)
- reimplement video.tile in native liq
- rework buffer.adaptative
- use source getters for switch in order to be able to play two tracks ever day
  (#2880)

### From lioquidshop 5:

- Better handling over core module erasure
- Switch stream callbacks to async first
- Add variable with current liq script name.

### Maybe TODO:

- remove requests and use sources instead everywhere (a request is a source with
  one track [or more]) (weak maybe)
  - Precise scheduling with queue.push, etc.: we could make the track available
    at some precise time if requests were sources...
  - this may allow stuff like `append` more easily
- Add support for modules, load minimal API by default
- Simple mechanism to tell source how much data will be expected in advance (e.g. 10s with cross) to allow automatic buffer management.
- Redefine switch-based transitions.

### Nice to have

- refine video support in order to have next liquidshop running on Liquidsoap (dogfooding)
- use row variables for methods, using Garrigue's _Simple Type Inference for Structural Polymorphism_
- can we reimplement something like [melt](https://www.mltframework.org/)?
- support for WebRTC using WHIP / WHEP
- support decorations on a subtitle image track
- make bindings to pipewire to support webcams and screensharing

## For 2.2

### Done

- ~~Separate language core (#2397)~~
- ~~Online version (#2397)~~
  - ~~Available at: https://www.liquidsoap.info/try/~~
  - ~~Needs some cleanup, definition of a minimal JS library.~~
- ~~Switch to `dune`~~
- ~~Separate standard library (in pure liq)~~
- ~~support for multi-track audio~~
- ~~live switch with ffmpeg encoded content~~
- ~~deprecate "!" and ":=" in favor of x.get / x.set~~
- ~~switch to immutable content for metadata~~
- ~~Add script tooling, prettier etc.~~
- ~~switch to immutable content for frames (#2364)~~
  - ~~frame should be changed to extensible arrays (a bit like `Strings`) instead of filling a buffer~~
  - ~~take the opportunity to change the handling of track boundaries (currently
    boundary = we have a partial fill, which has quite messy corner cases)~~

## For 2.3

### Done:

- ~~Rewrite streaming loop~~
- ~~rewrite the clock system~~
  - ~~the code is unreadable and overengineered ⇒ simplify it~~
  - we want to get rid of the assumption clock = thread (Feasible but problem with OCaml 5)
- ~~Optimize runtime: start time, typing and memory usage~~
- ~~javascrtipt/browser support using [WebCodecs](https://developer.mozilla.org/en-US/docs/Web/API/WebCodecs_API)!~~
