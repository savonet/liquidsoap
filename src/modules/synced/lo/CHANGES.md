# 0.2.1 (2026-10-07)

- Fix dangling GC roots in the server handler when the handler raises or an
  unhandled message type is received.
- Detect liblo through `pkg-config`; now depends on `conf-pkg-config`.
- Require dune 3.23 or later.
- Clarify license by switching to plain LGPL 2.1+, without any exception (#5).

# 0.2.0 (2021-03-13)

- Switch to dune.

# 0.1.2 (2018-06-26)

- Add `Server.stop` function.

# 0.1.1 (2015-08-03)

- Dummy github release.

# 0.1.0 (2011-07-04)

- Initial release.
