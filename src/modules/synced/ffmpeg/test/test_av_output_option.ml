(* Conformance of Av.set_output_option: spec/tests.md §3, requirement 3.27. *)

open Avutil
open Harness

let packet_size = 188

(* Transport packets of PID 0 that start a section. *)
let pat_count content =
  let count = ref 0 in
  for packet = 0 to (String.length content / packet_size) - 1 do
    let offset = packet * packet_size in
    let byte index = Char.code content.[offset + index] in
    if
      byte 0 = 0x47
      && byte 1 land 0x40 <> 0
      && byte 1 land 0x1f = 0
      && byte 2 = 0
    then incr count
  done;
  !count

let requirement_3_27 () =
  let written = Buffer.create 65536 in
  let write bytes offset length =
    Buffer.add_subbytes written bytes offset length;
    length
  in
  let output =
    Av.open_output_stream
      ~opts:
        (Test_av.opts
           [
             ("pat_period", `String "1000000"); ("sdt_period", `String "1000000");
           ])
      write (Test_av.mpegts ())
  in
  (* A key video packet after a non-key one also writes the tables. *)
  let stream = Test_av.video_stream ~bindings:[("g", `Int 300)] output in
  Av.set_output_option output "mpegts_flags" "+latm";
  for index = 0 to 49 do
    Av.write_frame stream (Test_av.picture index)
  done;
  Av.flush output;
  let before = Buffer.contents written in
  equal 0 (String.length before mod packet_size) "whole packets are flushed";
  equal 1 (pat_count before)
    "only the initial table, with a long period and one key packet";
  Av.set_output_option output "mpegts_flags" "+resend_headers";
  for index = 50 to 99 do
    Av.write_frame stream (Test_av.picture index)
  done;
  Av.flush output;
  let after =
    Buffer.sub written (String.length before)
      (Buffer.length written - String.length before)
  in
  check (String.length after > 0) "packets follow the call";
  equal 1 (pat_count after) "resend_headers writes the table once more";
  raises
    (function Error `Option_not_found -> true | _ -> false)
    "an unknown option"
    (fun () -> Av.set_output_option output "no_such_option" "1");
  raises Test_av.is_error "an invalid value" (fun () ->
      Av.set_output_option output "pat_period" "not a number");
  Av.close output;
  raises Test_av.is_closed "a closed output" (fun () ->
      Av.set_output_option output "mpegts_flags" "+resend_headers")

let requirements = [("3.27", requirement_3_27)]
