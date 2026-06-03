open Ctypes
module Constants = Srt_constants.Def (Srt_generated_constants)
include Constants

module Def (S : Cstubs.Types.TYPE) = struct
  type socket = int

  let const_string = typedef string "const char*"

  module PollEvent = struct
    type t = unit

    let t : t structure S.typ = S.structure "SRT_EPOLL_EVENT_STR"
    let t = S.typedef t "SRT_EPOLL_EVENT"
    let fd = S.field t "fd" S.int
    let events = S.field t "events" S.int
    let () = S.seal t
  end

  module CBytePerfMon = struct
    type t = unit

    let t : t structure S.typ = S.structure "CBytePerfMon"
    let msTimeStamp = S.field t "msTimeStamp" S.int64_t
    let pktSentTotal = S.field t "pktSentTotal" S.int64_t
    let pktRecvTotal = S.field t "pktRecvTotal" S.int64_t
    let pktSndLossTotal = S.field t "pktSndLossTotal" S.int
    let pktRcvLossTotal = S.field t "pktRcvLossTotal" S.int
    let pktRetransTotal = S.field t "pktRetransTotal" S.int
    let pktSentACKTotal = S.field t "pktSentACKTotal" S.int
    let pktRecvACKTotal = S.field t "pktRecvACKTotal" S.int
    let pktSentNAKTotal = S.field t "pktSentNAKTotal" S.int
    let pktRecvNAKTotal = S.field t "pktRecvNAKTotal" S.int
    let usSndDurationTotal = S.field t "usSndDurationTotal" S.int64_t
    let pktSndDropTotal = S.field t "pktSndDropTotal" S.int
    let pktRcvDropTotal = S.field t "pktRcvDropTotal" S.int
    let pktRcvUndecryptTotal = S.field t "pktRcvUndecryptTotal" S.int
    let byteSentTotal = S.field t "byteSentTotal" S.uint64_t
    let byteRecvTotal = S.field t "byteRecvTotal" S.uint64_t

    (* Not sure how to conditionally enable struct members at the moment..
       let byteRcvLossTotal = S.field t "byteRcvLossTotal" S.uint64_t
    *)

    let byteRetransTotal = S.field t "byteRetransTotal" S.uint64_t
    let byteSndDropTotal = S.field t "byteSndDropTotal" S.uint64_t
    let byteRcvDropTotal = S.field t "byteRcvDropTotal" S.uint64_t
    let byteRcvUndecryptTotal = S.field t "byteRcvUndecryptTotal" S.uint64_t
    let pktSent = S.field t "pktSent" S.int64_t
    let pktRecv = S.field t "pktRecv" S.int64_t
    let pktSndLoss = S.field t "pktSndLoss" S.int
    let pktRcvLoss = S.field t "pktRcvLoss" S.int
    let pktRetrans = S.field t "pktRetrans" S.int
    let pktRcvRetrans = S.field t "pktRcvRetrans" S.int
    let pktSentACK = S.field t "pktSentACK" S.int
    let pktRecvACK = S.field t "pktRecvACK" S.int
    let pktSentNAK = S.field t "pktSentNAK" S.int
    let pktRecvNAK = S.field t "pktRecvNAK" S.int
    let mbpsSendRate = S.field t "mbpsSendRate" S.double
    let mbpsRecvRate = S.field t "mbpsRecvRate" S.double
    let usSndDuration = S.field t "usSndDuration" S.int64_t
    let pktReorderDistance = S.field t "pktReorderDistance" S.int
    let pktRcvAvgBelatedTime = S.field t "pktRcvAvgBelatedTime" S.double
    let pktRcvBelated = S.field t "pktRcvBelated" S.int64_t
    let pktSndDrop = S.field t "pktSndDrop" S.int
    let pktRcvDrop = S.field t "pktRcvDrop" S.int
    let pktRcvUndecrypt = S.field t "pktRcvUndecrypt" S.int
    let byteSent = S.field t "byteSent" S.uint64_t
    let byteRecv = S.field t "byteRecv" S.uint64_t

    (* Not sure how to conditionally enable struct members at the moment..
       let byteRcvLoss = S.field t "byteRcvLoss" S.uint64_t
    *)

    let byteRetrans = S.field t "byteRetrans" S.uint64_t
    let byteSndDrop = S.field t "byteSndDrop" S.uint64_t
    let byteRcvDrop = S.field t "byteRcvDrop" S.uint64_t
    let byteRcvUndecrypt = S.field t "byteRcvUndecrypt" S.uint64_t
    let usPktSndPeriod = S.field t "usPktSndPeriod" S.double
    let pktFlowWindow = S.field t "pktFlowWindow" S.int
    let pktCongestionWindow = S.field t "pktCongestionWindow" S.int
    let pktFlightSize = S.field t "pktFlightSize" S.int
    let msRTT = S.field t "msRTT" S.double
    let mbpsBandwidth = S.field t "mbpsBandwidth" S.double
    let byteAvailSndBuf = S.field t "byteAvailSndBuf" S.int
    let byteAvailRcvBuf = S.field t "byteAvailRcvBuf" S.int
    let mbpsMaxBW = S.field t "mbpsMaxBW" S.double
    let byteMSS = S.field t "byteMSS" S.int
    let pktSndBuf = S.field t "pktSndBuf" S.int
    let byteSndBuf = S.field t "byteSndBuf" S.int
    let msSndBuf = S.field t "msSndBuf" S.int
    let msSndTsbPdDelay = S.field t "msSndTsbPdDelay" S.int
    let pktRcvBuf = S.field t "pktRcvBuf" S.int
    let byteRcvBuf = S.field t "byteRcvBuf" S.int
    let msRcvBuf = S.field t "msRcvBuf" S.int
    let msRcvTsbPdDelay = S.field t "msRcvTsbPdDelay" S.int
    let pktSndFilterExtraTotal = S.field t "pktSndFilterExtraTotal" S.int
    let pktRcvFilterExtraTotal = S.field t "pktRcvFilterExtraTotal" S.int
    let pktRcvFilterSupplyTotal = S.field t "pktRcvFilterSupplyTotal" S.int
    let pktRcvFilterLossTotal = S.field t "pktRcvFilterLossTotal" S.int
    let pktSndFilterExtra = S.field t "pktSndFilterExtra" S.int
    let pktRcvFilterExtra = S.field t "pktRcvFilterExtra" S.int
    let pktRcvFilterSupply = S.field t "pktRcvFilterSupply" S.int
    let pktRcvFilterLoss = S.field t "pktRcvFilterLoss" S.int

    (* Looks like this one is also too recent for our current set of supported
          platforms:
       let pktReorderTolerance = S.field t "pktReorderTolerance" S.int
    *)
    let () = S.seal t
  end
end
