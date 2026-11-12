# 0.4.0 (unreleased)

- Breaking: `Error` carries the system error behind a failure, when there is
  one: `Error of errno * Unix.error option * string`.
- Require OCaml 5.1.
- Added `tsbpdmode`, `getrejectreason` and `live_max_payload_size`.
- Fix `peerlatency`, which raised `Not_found`.
- Fix `Poll.update_usock` dropping the socket from `Poll.sockets`.
- Fix concurrent `close` and `listen_callback` racing on the callback table.
- Fix heap corruption: buffers passed to libsrt calls, which run without the
  runtime lock, could be freed by a collection on another thread before the
  call returned.
- Fix reading the `streamid` option, which always failed for a non-empty id.
- Log handler: append in constant time, so a log flood does not stall
  libsrt's threads; fix the copied `area` and `line` fields.

  # 0.3.4 (2025-05-01)

- Copy send message before releasing the global lock.
- Use fixed posix socket API for allocating socket address
  storage.
- Added `accept_no_origin`.

  # 0.3.3 (2025-01-30)

- Add support for `SRTO_LATENCY` and `SRTO_PEERLATENCY`

  # 0.3.2 (2025-01-16)

- cross-compilation: use `wine` CLI when `wine64`
  is not available.
- Add support for `ipv6only` socket option.
- Add API to use posix sockets for connecting.

  # 0.3.1 (2024-10-29)

- Added missing dependency on `ocamlfind`
- Start/stop log processing on `startup`/`cleanup`.

  # 0.3.0 (2022-03-04)

- Added `rcvdata` and `rcvlatency`
- Added read/write constraints to socket options.
- Remove `max_*` arguments in polling functions.

  # 0.2.2 (2022-09-21)

- Reimplement log handler to be non-blocking and
  outside of the OCaml heap.
- Added `listen_callback` API
- Added `pbkeylen`, `passphrase` and `streamid` socket options.

  # 0.2.1 (2022-01-02)

- Added support for conn/rcn/sndtimeo.

  # 0.2.0 (2021-06-18)

- Added support for uwait polling.
- Added support for stats.
- Remove deprecated `srt_socket`

  # 0.1.1 (2020-06-13)

- Switch to `posix-socket`
- Added support for `SRTO_ENFORCEDENCRYPTION`

  # 0.1.0 (2019-09-17)

- Initial release
