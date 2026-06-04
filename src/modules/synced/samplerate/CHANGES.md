# 0.1.7 (unreleased\_

- Clip returned values, except in big array API.

  # 0.1.6 (02-07-2021)

* Change licence to 2-clause BSD in order to match libsamplerate's licence (#1).
* Fix various memory leaks due to incorrect handling of channels (#2). All
  offsets are sizes should now be in number of _frames_, i.e. number of samples
  _per channel_.

  # 0.1.5 (14-11-2020)

* Switched to dune
* Add `process_ba`.

  # 0.1.4 (15-10-2016)

* Lift configure failure when compiling as root.

  # 0.1.3 (03-08-2015)

* Dummy github release.

  # 0.1.2 (18-20-2013)

* Cleanly handle case where input buffer has length 0.
* Check for memory allocation errors.
* Updated configure.

  # 0.1.1 (12-10-2009)

* Added support for --enable-debugging configure option
* Added NO_CUSTOM to build
  in standard mode.
* Added prefix to main compilation variables
  if passed to configure.
* Makefile now honnors LIBDIRS
  variable for linking against libraries
  located in other places than then standard
  ones.

  # 0.1.0 (17/02/2009)

* Initial release
