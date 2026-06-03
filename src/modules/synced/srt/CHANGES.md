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
