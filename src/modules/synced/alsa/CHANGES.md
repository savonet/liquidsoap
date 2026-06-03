# 0.3.1 (unreleased)

- Add `writei_floatn`.
- Add `device_name_hints`.

  # 0.3.0 (2020-08-08)

- Switch to `bytes` for I/O operations (makes the API incompatible with previous
  versions).
- Switch to `dune` for building.
- Add `readn_float_ba` and `writen_float_ba`.
- Start adding support for sequencers (`Sequencer` module).

  # 0.2.3 (2016-11-18)

- Fix `CAMLlocal` bug (#1).

  # 0.2.2 (2015-07-29)

- Dummy github release.

  # 0.2.1 (2010-05-12)

- Use `snd_pcm_sframes_t` for error codes to avoid int overflow. Thanks to
  Peter Retep for reporting and patching!
- Added `get_periods_{min,max}` and `get_buffer_max`.

  # 0.2.0 (2010-08-19)

- Added resume and recover functions.
- Cleaned exception handling.

  # 0.1.4 (2008-10-11)

- Added support for `--enable-debugging` configure option
- Added NO_CUSTOM to build in standard mode.
- Added prefix to main compilation variables if passed to configure.
- Makefile now honnors `LIBDIRS` variable for linking against libraries located
  in other places than then standard ones.

  # 0.1.3 (2008-04-14)

- Install .cmx file with others

  # 0.1.2 (2007-10-17)

- Added `readn` and `writen`.
- Improved configure support.

  # 0.1.1 (2007-05-22)

- Better error reporting.
- Added `set_nonblock`.
- Added `{readn,writen}_float{,64}` functions to access devices directly using
  floats.
- Added get_version.
- Correctly install on non-native architectures (for OCaml).

  # 0.1.0 (2006-07-18)

- Initial release.
