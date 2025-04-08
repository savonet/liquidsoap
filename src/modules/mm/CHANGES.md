# 0.8.6 (2024-10-29)

- Added FLTP module.

  # 0.8.5 (2024-02-05)

- Fix note names (#18).
- Fix GC pace when allocating bigarrays with custom memory.
- Fix UV height calculation in `caml_yuv420_fill`.

  # 0.8.4 (2023-07-01)

- Add `Image.ARGB8.*`.
- Add conversion functions to/from int16 big arrays.

  # 0.8.3 (2023-03-01)

- Add `Image.RGB8.Color.to_int`.
- Add `Image.Canvas.planes`.
- Add `Image.Invalid_position` and `Image.Invalid_dimensions` exceptions.

  # 0.8.2 (2023-02-08)

- Add ReplayGain computation.

  # 0.8.1 (24-05-2022)

- Add support for bitmaps and bitmap fonts.
- Working AVI video output.
- Compile with OCaml 5.
- Clarify license by switching to plain LGPL 2.1 (#16).

  # 0.8.0 (13-03-2022)

- Add support for image and video canvas.
- Switch audio data type to `float array`

  # 0.7.5 (03-02-2022)

* Add `alpha_of_color` in YUV.

  # 0.7.4 (28-01-2022)

* Implement low-level audio manipulation in C
  to avoid needless allocations.
* Add Audio.Mono.squares
* Cleanup aligned memory allocation.

  # 0.7.3 (10-12-2021)

* Remove usage of \_\_mingw_aligned_malloc as it needs a special free call.

  # 0.7.2 (22-11-2021)

* Fix offsets in to\_{s16, u8} functions.
* Switch to `aligned_alloc` for allocate aligned
  memory, fix minor heap stats when allocating
  bigarrays with aligned memory. (ocaml/ocaml#10788)

  # 0.7.1 (10-01-2021)

* Use only our own custom byte swap implementations.

  # 0.7.0 (04-01-2021)

* Switch to dune!
* Add set_alpha
* Add box_alpha

  # 0.6.0 (12-10-2020)

- Use `YUV420` for video frames.
- Use bigarrays to implement mono audio buffers, should be more efficient.
- Add `Image.Generic.blank`.
- Add `scale` and `disk` effects on alpha channel for YUV420.
- Make sure `to_mono` initializes an audio array with zeroes.

  # 0.5.0 (18-08-2019)

* New implementation of `YUV420`, added many function to manipulate those.
* Enhanced `Video` module.

  # 0.4.1 (27-06-2019)

* Fix memory leak in `RGBA32.of_RGB24_string`.
* Add `YUV420.of_string`.

  # 0.4.0 (18-08-2018)

* Use bytes instead of strings whenever appropriate.

  # 0.3.1 (16-10-2017)

* Fixed compilation with OCaml 4.06

  # 0.3.0 (03-08-2015)

* Add support for S16BE, S24LE and S32LE
* Fix deprecated APIs

  # 0.2.1 (2013-02-18)

* Add pulseaudio backend.
* Add channel and rate parameters for AO.
* Add resampling mode (`Nearest or `Linear).
* Remove on-the-fly samplerate conversions which were of bad quality (please
  use a proper resampler such as ocaml-samplerate instead).
* Handle BGRA format.
* Check for memory allocation failures.
* Add a video player example.

  # 0.2.0 (2011-10-04)

* Add alpha channel for drawn lines.
* Improved autoconf.

  # 0.1.0 (2011-06-30)

* Initial release.
