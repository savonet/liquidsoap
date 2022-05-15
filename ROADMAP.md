2022-05-13
----------

- refine video support in order to have next liquidshop running on Liquidsoap
  (dogfooding)
- native RTMP support (and ensure that HLS output is easy to use)
- live switch with ffmpeg encoded content
- support for multi-track audio
- support for subtitles
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
- switch to dune (if it makes sense)
- separate standard library (in pure liq), might be easier with dune
- separate langage core (#2397)
  - we need to remove formats (add them separately with += in types)
  - can we have extensible parsers
- online version (#2397)
  - we first need the separate langage
  - we cannot use threads (unless we use lwt) and all libraries (dtools [which
    uses threads], pcre, mm)
- modules (#1934 based on runtime types, not ideal): we can implement them as
  records
  - import
  - support for hiding fields
- ocaml5 support
  - we don't seem to have bigarrays anymore?
