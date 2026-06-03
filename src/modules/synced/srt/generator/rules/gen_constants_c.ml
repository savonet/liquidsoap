let c_headers =
  {|
#include <srt/srt.h>

#ifndef SRT_ENABLE_LOSTBYTESCOUNT
#define SRT_ENABLE_LOSTBYTESCOUNT 0
#endif

#if (SRT_VERSION_MAJOR <= 1) && (SRT_VERSION_MINOR <= 4) && (SRT_VERSION_PATCH <= 1)
#define SRT_EPOLLEMPTY SRT_EUNKNOWN
#define SRT_ESCLOSED SRT_EUNKNOWN
#define SRT_ESYSOBJ SRT_EUNKNOWN
#endif
|}

let () =
  let fname = Sys.argv.(1) in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  Format.fprintf format "%s@\n" c_headers;
  Cstubs.Types.write_c format (module Srt_constants.Def);
  Format.pp_print_flush format ();
  close_out oc
