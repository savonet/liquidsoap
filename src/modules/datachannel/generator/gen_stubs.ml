let c_headers =
  {|
#include <rtc/rtc.h>

/* ctypes generates int where the C API uses typedef'd anonymous enums
   (rtcState, rtcGatheringState) and char* where const char* is expected.
   These are ABI-compatible, so suppress the strict pointer check. */
#pragma GCC diagnostic ignored "-Wincompatible-pointer-types"
#pragma GCC diagnostic ignored "-Wincompatible-function-pointer-types"
|}

let () =
  let mode = Sys.argv.(1) in
  let fname = Sys.argv.(2) in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  let fn =
    match mode with
      | "ml" -> Cstubs.write_ml
      | "c" ->
          Format.fprintf format "%s@\n" c_headers;
          Cstubs.write_c
      | _ -> assert false
  in
  fn ~concurrency:Cstubs.unlocked format ~prefix:"ocaml_datachannel"
    (module Datachannel_stubs.Def);
  Format.pp_print_flush format ();
  close_out oc
