# `srt.socket`: the script socket value

## 1. The value

A script sees an SRT socket as a value of type `srt_socket`. It prints as
`<srt_socket>`, compares by the underlying socket handle, and converting it to
JSON raises an error of kind `json` with the message
`SRT socket cannot be represented as json`.

`srt.socket(s)` returns the same socket carrying the methods below. Where a
socket reaches a script:

| Where                                                            | Methods present                |
| ---------------------------------------------------------------- | ------------------------------ |
| `on_socket` callback ([common.md §3](common.md#3-callbacks))     | yes, though typed `srt_socket` |
| `listen_callback` ([listener.md §3](listener.md#3-socket-setup)) | no                             |
| `sockets()` ([common.md §4](common.md#4-methods))                | no                             |

Every method but `id` calls libsrt; a libsrt error is raised to the script as
an error of kind `srt`.

## 2. Options

For each readable option, a getter `name()`; for each writable option, a
setter `set_name(v)`.

| Name                  | libsrt option          | Type   | Access     |
| --------------------- | ---------------------- | ------ | ---------- |
| `rcvdata`             | receive data available | int    | read       |
| `messageapi`          | message API            | bool   | write      |
| `payloadsize`         | payload size           | int    | write      |
| `conntimeo`           | connect timeout, ms    | int    | write      |
| `passphrase`          | passphrase             | string | write      |
| `enforced_encryption` | enforced encryption    | bool   | write      |
| `rcvsyn`              | blocking receive       | bool   | read/write |
| `sndsyn`              | blocking send          | bool   | read/write |
| `rcvtimeout`          | receive timeout, ms    | int    | read/write |
| `sndtimeout`          | send timeout, ms       | int    | read/write |
| `reuseaddr`           | reuse address          | bool   | read/write |
| `rcvbuf`              | receive buffer         | int    | read/write |
| `sndbuf`              | send buffer            | int    | read/write |
| `udp_rcvbuf`          | UDP receive buffer     | int    | read/write |
| `udp_sndbuf`          | UDP send buffer        | int    | read/write |
| `streamid`            | stream id              | string | read/write |
| `pbkeylen`            | key length             | int    | read/write |
| `ipv6only`            | IPv6 only              | bool   | read/write |
| `rcvlatency`          | receive latency, ms    | int    | read/write |
| `peerlatency`         | peer latency, ms       | int    | read/write |
| `latency`             | latency, ms            | int    | read/write |

`rcvtimeout` and `sndtimeout` are the libsrt options `rcvtimeo` and
`sndtimeo`. The transport type cannot be read or set from a script.

## 3. Other methods

| Method                                    | Result                                                                                                                 |
| ----------------------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `id`                                      | int, the libsrt socket id (a value, not a function).                                                                   |
| `status()`                                | One of `initialized`, `opened`, `listening`, `connecting`, `connected`, `broken`, `closing`, `closed`, `non_existant`. |
| `close()`                                 | Closes the socket. The operator holding it is not told.                                                                |
| `bstats(clear=null)`                      | Statistics record, §4.                                                                                                 |
| `bistats(clear=null, instantaneous=null)` | Statistics record, §4.                                                                                                 |

Null leaves the libsrt default for `clear` and `instantaneous`.

## 4. Statistics

A record with one field per libsrt statistic, named as libsrt names them.
Integer statistics are converted to the native integer, whatever their width
or sign in libsrt. Fields marked (float) are floats.

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
