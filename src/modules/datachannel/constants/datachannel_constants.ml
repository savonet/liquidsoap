module Def (S : Cstubs.Types.TYPE) = struct
  open S

  let rtc_new = constant "RTC_NEW" int64_t
  let rtc_connecting = constant "RTC_CONNECTING" int64_t
  let rtc_connected = constant "RTC_CONNECTED" int64_t
  let rtc_disconnected = constant "RTC_DISCONNECTED" int64_t
  let rtc_failed = constant "RTC_FAILED" int64_t
  let rtc_closed = constant "RTC_CLOSED" int64_t
  let rtc_gathering_new = constant "RTC_GATHERING_NEW" int64_t
  let rtc_gathering_inprogress = constant "RTC_GATHERING_INPROGRESS" int64_t
  let rtc_gathering_complete = constant "RTC_GATHERING_COMPLETE" int64_t
  let rtc_log_none = constant "RTC_LOG_NONE" int64_t
  let rtc_log_fatal = constant "RTC_LOG_FATAL" int64_t
  let rtc_log_error = constant "RTC_LOG_ERROR" int64_t
  let rtc_log_warning = constant "RTC_LOG_WARNING" int64_t
  let rtc_log_info = constant "RTC_LOG_INFO" int64_t
  let rtc_log_debug = constant "RTC_LOG_DEBUG" int64_t
  let rtc_log_verbose = constant "RTC_LOG_VERBOSE" int64_t
  let rtc_direction_unknown = constant "RTC_DIRECTION_UNKNOWN" int64_t
  let rtc_direction_sendonly = constant "RTC_DIRECTION_SENDONLY" int64_t
  let rtc_direction_recvonly = constant "RTC_DIRECTION_RECVONLY" int64_t
  let rtc_direction_sendrecv = constant "RTC_DIRECTION_SENDRECV" int64_t
  let rtc_direction_inactive = constant "RTC_DIRECTION_INACTIVE" int64_t
  let rtc_codec_h264 = constant "RTC_CODEC_H264" int64_t
  let rtc_codec_vp8 = constant "RTC_CODEC_VP8" int64_t
  let rtc_codec_vp9 = constant "RTC_CODEC_VP9" int64_t
  let rtc_codec_h265 = constant "RTC_CODEC_H265" int64_t
  let rtc_codec_av1 = constant "RTC_CODEC_AV1" int64_t
  let rtc_codec_opus = constant "RTC_CODEC_OPUS" int64_t
  let rtc_codec_pcmu = constant "RTC_CODEC_PCMU" int64_t
  let rtc_codec_pcma = constant "RTC_CODEC_PCMA" int64_t
  let rtc_codec_aac = constant "RTC_CODEC_AAC" int64_t
end
