(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(* Test that Process_handler correctly captures all stdout data even when
   a process outputs a large amount of data and exits immediately.

   Usage:
     process_handler_test.exe --generate-output <size>
       Outputs <size> bytes of 'x' characters to stdout and exits.

     process_handler_test.exe
       Runs the actual test using Process_handler. *)

(* Size of test output - should be large enough to potentially overflow
   pipe buffers and trigger the race condition if present *)
let output_size = 1_000_000

let generate_output size =
  let chunk_size = 4096 in
  let chunk = String.make chunk_size 'x' in
  let remaining = ref size in
  while !remaining > 0 do
    let to_write = min !remaining chunk_size in
    output_substring stdout chunk 0 to_write;
    remaining := !remaining - to_write
  done;
  flush stdout

let run_test () =
  let m = Mutex.create () in
  let finished = Condition.create () in
  let collected_output = Buffer.create 1024 in
  let test_finished = ref false in
  let on_stdout pull =
    let buf = Bytes.create 4096 in
    let n = pull buf 0 4096 in
    Buffer.add_subbytes collected_output buf 0 n;
    `Continue
  in
  let on_stop _ =
    Mutex.lock m;
    test_finished := true;
    Condition.signal finished;
    Mutex.unlock m;
    false
  in
  Tutils.start ();
  let exe = Sys.executable_name in
  let command = Printf.sprintf "%s --generate-output %d" exe output_size in
  ignore
    (Process_handler.run ~on_stdout ~on_stop
       ~log:(fun msg -> Printf.eprintf "Process: %s\n%!" msg)
       command);
  Mutex.lock m;
  while not !test_finished do
    Condition.wait finished m
  done;
  Mutex.unlock m;
  let collected = Buffer.contents collected_output in
  let collected_len = String.length collected in
  if collected_len <> output_size then begin
    Printf.eprintf
      "FAIL: Expected %d bytes, got %d bytes (missing %d bytes)\n%!" output_size
      collected_len
      (output_size - collected_len);
    Tutils.shutdown 1
  end;
  Printf.printf "OK: Received all %d bytes of output\n%!" collected_len;
  Tutils.shutdown 0

let () =
  match Array.to_list Sys.argv with
    | [_; "--generate-output"; size] -> generate_output (int_of_string size)
    | [_] -> run_test ()
    | _ ->
        Printf.eprintf "Usage: %s [--generate-output <size>]\n" Sys.argv.(0);
        exit 1
