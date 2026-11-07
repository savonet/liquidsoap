# `srt.socket`: the script socket value

## 1. The value

A script sees an SRT socket as a value of type `srt_socket`. It prints as
`<srt_socket>`, compares by the underlying socket handle, and converting it to
JSON raises an error of kind `json` with the message
`SRT socket cannot be represented as json`.

Wherever a socket reaches a script (`on_socket`, `listen_callback`,
`sockets()`), it carries the methods below and is typed with them.
`srt.socket(s)` returns the same socket with its methods, for a value typed
without them.

Every method but `id` calls libsrt; a libsrt error, including one from a
socket already closed, is raised to the script as an error of kind `srt`.

## 2. Options

For each readable option, a getter `name()`; for each writable option, a
setter `set_name(v)`.

| Name                  | libsrt option            | Type   | Access     |
| --------------------- | ------------------------ | ------ | ---------- |
| `rcvdata`             | receive data available   | int    | read       |
| `messageapi`          | message API              | bool   | write      |
| `payloadsize`         | payload size             | int    | write      |
| `conntimeo`           | connect timeout, ms      | int    | write      |
| `passphrase`          | passphrase               | string | write      |
| `enforced_encryption` | enforced encryption      | bool   | write      |
| `rcvsyn`              | blocking receive         | bool   | read/write |
| `sndsyn`              | blocking send            | bool   | read/write |
| `rcvtimeout`          | receive timeout, ms      | int    | read/write |
| `sndtimeout`          | send timeout, ms         | int    | read/write |
| `reuseaddr`           | reuse address            | bool   | read/write |
| `rcvbuf`              | receive buffer           | int    | read/write |
| `sndbuf`              | send buffer              | int    | read/write |
| `udp_rcvbuf`          | UDP receive buffer       | int    | read/write |
| `udp_sndbuf`          | UDP send buffer          | int    | read/write |
| `streamid`            | stream id                | string | read/write |
| `pbkeylen`            | key length               | int    | read/write |
| `ipv6only`            | IPv6 only                | bool   | read/write |
| `rcvlatency`          | receive latency, ms      | int    | read/write |
| `peerlatency`         | peer latency, ms         | int    | read/write |
| `latency`             | latency, ms              | int    | read/write |
| `tsbpdmode`           | timestamp-based delivery | bool   | read/write |

`rcvtimeout` and `sndtimeout` are the libsrt options `rcvtimeo` and
`sndtimeo`. The transport type cannot be read or set from a script.

The blocking mode and timeouts of a socket an operator uses for data are the
operator's ([concurrency.md §1](concurrency.md#1-threads)): a script that
changes them on such a socket may make the operator fail that peer.

## 3. Other methods

| Method                                    | Result                                                                                                                 |
| ----------------------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `id`                                      | int, the libsrt socket id (a value, not a function).                                                                   |
| `status()`                                | One of `initialized`, `opened`, `listening`, `connecting`, `connected`, `broken`, `closing`, `closed`, `non_existant`. |
| `close()`                                 | Closes the socket.                                                                                                     |
| `bstats(clear=null)`                      | Statistics record, §4.                                                                                                 |
| `bistats(clear=null, instantaneous=null)` | Statistics record, §4.                                                                                                 |

Null leaves the libsrt default for `clear` and `instantaneous`.

A script that closes a socket an operator holds ends that peer as a failure,
which the operator sees at its next exchange
([connection.md §7](connection.md#7-peer-failure)). The operator still fires
the `"close"` event for it once, when it releases the peer.

## 4. Statistics

A record with one field per libsrt statistic, named as libsrt names them.
Integer statistics are converted to the native integer, saturating at its
bounds. Fields marked (float) are floats.

Fields: `msTimeStamp`, `pktSentTotal`, `pktRecvTotal`, `pktSndLossTotal`,
`pktRcvLossTotal`, `pktRetransTotal`, `pktSentACKTotal`, `pktRecvACKTotal`,
`pktSentNAKTotal`, `pktRecvNAKTotal`, `usSndDurationTotal`, `pktSndDropTotal`,
`pktRcvDropTotal`, `pktRcvUndecryptTotal`, `byteSentTotal`, `byteRecvTotal`,
`byteRetransTotal`, `byteSndDropTotal`, `byteRcvDropTotal`,
`byteRcvUndecryptTotal`, `pktSent`, `pktRecv`, `pktSndLoss`, `pktRcvLoss`,
`pktRetrans`, `pktRcvRetrans`, `pktSentACK`, `pktRecvACK`, `pktSentNAK`,
`pktRecvNAK`, `mbpsSendRate` (float), `mbpsRecvRate` (float),
`usSndDuration`, `pktReorderDistance`, `pktRcvAvgBelatedTime` (float),
`pktRcvBelated`, `pktSndDrop`, `pktRcvDrop`, `pktRcvUndecrypt`, `byteSent`,
`byteRecv`, `byteRetrans`, `byteSndDrop`, `byteRcvDrop`, `byteRcvUndecrypt`,
`usPktSndPeriod` (float), `pktFlowWindow`, `pktCongestionWindow`,
`pktFlightSize`, `msRTT` (float), `mbpsBandwidth` (float), `byteAvailSndBuf`,
`byteAvailRcvBuf`, `mbpsMaxBW` (float), `byteMSS`, `pktSndBuf`, `byteSndBuf`,
`msSndBuf`, `msSndTsbPdDelay`, `pktRcvBuf`, `byteRcvBuf`, `msRcvBuf`,
`msRcvTsbPdDelay`, `pktSndFilterExtraTotal`, `pktRcvFilterExtraTotal`,
`pktRcvFilterSupplyTotal`, `pktRcvFilterLossTotal`, `pktSndFilterExtra`,
`pktRcvFilterExtra`, `pktRcvFilterSupply`, `pktRcvFilterLoss`.
