(* Bug #22 *)

open Mm_midi

let () =
  let fname = "output.mid" in
  let writer = new MIDI.IO.Writer.to_file 44100 fname in
  writer#note_on 1 64 1.0;
  writer#advance 44100;
  writer#note_off 1 64 0.0;
  writer#close
(* let reader = new MIDI.IO.Reader.of_file fname in *)
(* reader#read 0 *)
