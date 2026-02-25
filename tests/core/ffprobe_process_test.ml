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

(* Test that Process_handler correctly captures ffprobe JSON output.
   This tests a real-world use case with potentially large JSON output. *)

let m = Mutex.create ()
let finished = Condition.create ()
let collected_output = Buffer.create 1024
let test_finished = ref false

let on_stdout pull =
  let buf = Bytes.create 4096 in
  let n = pull buf 0 4096 in
  Buffer.add_subbytes collected_output buf 0 n;
  `Continue

let on_stop _ =
  Mutex.lock m;
  test_finished := true;
  Condition.signal finished;
  Mutex.unlock m;
  false

let () =
  let media_file =
    match Sys.argv with
      | [| _; file |] -> file
      | _ ->
          Printf.eprintf "Usage: %s <media_file>\n" Sys.argv.(0);
          exit 1
  in
  if not (Sys.file_exists media_file) then begin
    Printf.eprintf "FAIL: Media file not found: %s\n%!" media_file;
    exit 1
  end;
  Printf.printf "Testing ffprobe on: %s\n%!" media_file;
  Tutils.start ();
  let command =
    Printf.sprintf
      "ffprobe -v quiet -print_format json -show_format -show_streams %s"
      (Filename.quote media_file)
  in
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
  Printf.printf "Received %d bytes of output\n%!" collected_len;
  if collected_len = 0 then begin
    Printf.eprintf "FAIL: No output received from ffprobe\n%!";
    Tutils.shutdown 1
  end;
  (* Parse as JSON to verify completeness *)
  let json =
    match Json.from_string collected with
      | json -> json
      | exception Json.Parse_error { message; _ } ->
          Printf.eprintf "FAIL: Invalid JSON output: %s\n%!" message;
          Printf.eprintf "Output was:\n%s\n%!" collected;
          Tutils.shutdown 1;
          assert false
  in
  (* Verify expected ffprobe structure *)
  (match json with
    | `Assoc fields ->
        if not (List.mem_assoc "streams" fields) then begin
          Printf.eprintf "FAIL: JSON missing 'streams' field\n%!";
          Tutils.shutdown 1
        end;
        if not (List.mem_assoc "format" fields) then begin
          Printf.eprintf "FAIL: JSON missing 'format' field\n%!";
          Tutils.shutdown 1
        end
    | _ ->
        Printf.eprintf "FAIL: JSON is not an object\n%!";
        Tutils.shutdown 1);
  Printf.printf "OK: Received valid ffprobe JSON output (%d bytes)\n%!"
    collected_len;
  Tutils.shutdown 0
